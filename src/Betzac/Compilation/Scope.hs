{-# LANGUAGE OverloadedStrings #-}

module Betzac.Compilation.Scope (
    Candidate (..),
    labelText,
    exportedScope,
    localDefs,
    effectiveScope,
) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (
    AtomExpr (From),
    BetzaExpr (BetzaExpr),
    BetzaProgram,
    BetzaStmt (..),
    ChainExpr (ChainExpr),
    Directive (..),
    ExponentExpr (ExponentExpr),
    Label (..),
    ModifierExpr (ModifierExpr),
    QualifiedStmt (..),
    UnionExpr (UnionExpr),
 )
import Betzac.Compilation.Context (ExportedDef (..), ResolvedDef (..))
import Betzac.Semantic.Core (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Span (HasSpan (..), Span (..))
import Text.Megaparsec.Pos (SourcePos (..), unPos)

-- Note: this module resolves which definition wins per label. It does not yet track
-- whether a label/expression/using target is ever referenced, so unused-label,
-- unused-expression, and unused-file detection are not implemented here — planned as
-- follow-up work.

-- | Normalized text of a label, used as the key into scope maps.
labelText :: Label p -> String
labelText (Upper c _) = [c]
labelText (Descriptor s _) = s
labelText (Leaper m n _) = show m ++ "," ++ show n

{- | The literal source text of an expression, sliced via its span — used to build the
synthetic label for an exported anonymous statement.
-}
exprSourceText :: Text -> BetzaExpr Ps -> String
exprSourceText src expr = T.unpack $ sourceSlice src $ getSpan expr

sourceSlice :: Text -> Span -> Text
sourceSlice _ Generated = ""
sourceSlice src (RealSpan s e) =
    let ls = T.lines src
        (sl, sc) = (unPos (sourceLine s) - 1, unPos (sourceColumn s) - 1)
        (el, ec) = (unPos (sourceLine e) - 1, unPos (sourceColumn e) - 1)
     in if sl == el
            then T.take (ec - sc) (T.drop sc (ls !! sl))
            else
                T.intercalate
                    "\n"
                    ( T.drop sc (ls !! sl)
                        : take (el - sl - 1) (drop (sl + 1) ls)
                        ++ [T.take ec (ls !! el)]
                    )

{- | One statement in a file, tagged with whether it (or its directive) is wrapped in
an override, and whether it's exported.
-}
statementOf :: QualifiedStmt Ps -> Maybe (BetzaStmt Ps, Bool, Bool)
statementOf (Plain (Bare stmt _) _) = Just (stmt, False, False)
statementOf (Override (Bare stmt _) _) = Just (stmt, True, False)
statementOf (Plain (Export stmt _) _) = Just (stmt, False, True)
statementOf (Override (Export stmt _) _) = Just (stmt, True, True)
statementOf _ = Nothing

{- | If a statement is a bare label-resolving reference (@label;@ — a single atom,
with no chaining, modifiers, or exponent), the label it refers to. Such a statement
doesn't define anything new; it only points at a label defined elsewhere, so an
export of one re-exposes that label rather than introducing a fresh definition.
-}
bareLabelRef :: BetzaStmt p -> Maybe (Label p)
bareLabelRef (Anonymous (BetzaExpr (ChainExpr (UnionExpr (me :| []) _) Nothing _) _) _)
    | ModifierExpr False [] (ExponentExpr (From lbl _) Nothing _) _ <- me =
        Just lbl
bareLabelRef _ = Nothing

{- | The label a statement defines, if any. A bare label-resolving reference never
defines anything (see 'bareLabelRef'). Any other anonymous expression only defines
a (synthetic) label when exported; otherwise it's an ignored, unlabelled statement.
-}
labelOf :: Text -> Bool -> BetzaStmt Ps -> Maybe String
labelOf _ _ (Assign lbl _ _) = Just $ labelText lbl
labelOf _ _ stmt@(Anonymous _ _) | Just _ <- bareLabelRef stmt = Nothing
labelOf src True (Anonymous expr _) = Just $ exprSourceText src expr
labelOf _ False (Anonymous _ _) = Nothing

{- | All Assign-defined (or exported-anonymous) labels within a single file,
independent of whether they are exported — the candidate pool for that file's own
local scope.
-}
localDefs :: Text -> BetzaProgram Ps -> [ExportedDef]
localDefs src prog = mapMaybe (uncurry build) (zip [0 ..] prog)
  where
    build i qs = do
        (stmt, isOverride, isExported) <- statementOf qs
        lbl <- labelOf src isExported stmt
        return $ ExportedDef lbl stmt isOverride i

{- | One candidate definition contending for a label, tagged with its origin file, its
precedence class (0 = override, highest priority), and its position for
tie-breaking among candidates of equal class.
-}
data Candidate = Candidate
    { candFrom :: FilePath
    , candDef :: ExportedDef
    , candClass :: Int
    , candOrder :: (Int, Int)
    }

classOf :: Bool -> Int
classOf isOverride = if isOverride then 0 else 1

{- | Pick the winning candidate for a label by priority: override beats plain; among
equal precedence, earliest position wins.
-}
resolvePriority :: NonEmpty Candidate -> (Candidate, [Candidate])
resolvePriority cands = case sortOn (\c -> (candClass c, candOrder c)) (foldr (:) [] cands) of
    (w : ls) -> (w, ls)
    [] -> error "resolvePriority: unreachable, NonEmpty is never empty"

{- | The winning local definition for a label, by the same priority rule as
'resolvePriority' — used to resolve a bare label-resolving export against the
definition it refers to, rather than the (contentless) reference statement itself.
-}
localWinner :: [ExportedDef] -> String -> Maybe ExportedDef
localWinner locals lbl = case [d | d <- locals, edLabel d == lbl] of
    [] -> Nothing
    (d : ds) -> Just $ candDef $ fst $ resolvePriority $ fmap (\x -> Candidate "" x (classOf $ edIsOverride x) (0, edOrder x)) (d :| ds)

{- | The labels a file exposes to other files via export. A same-label export repeated
within one file yields only the highest-priority definition; the rest are ignored
and warned as a duplicate directive. A bare label-resolving export (@export A;@)
exposes whatever locally wins for that label, rather than exporting the reference
statement itself; if the label has no local definition at all, it's an unresolved
label.
-}
exportedScope :: Text -> BetzaProgram Ps -> ([ExportedDef], [SemanticProblem])
exportedScope src prog =
    let (defs, dupProbs) = Map.foldr collect ([], []) grouped
     in (defs, dupProbs ++ reverse refProbs)
  where
    locals = localDefs src prog

    (entries, refProbs) = foldl' step ([], []) (zip [0 :: Int ..] prog)

    step (accEntries, accProbs) (i, qs) = case statementOf qs of
        Just (stmt, isOverride, True) -> case bareLabelRef stmt of
            Just lbl -> case localWinner locals (labelText lbl) of
                Just def -> ((edLabel def, def) : accEntries, accProbs)
                Nothing -> (accEntries, mkProblem Error (UnresolvedLabel) (getSpan stmt) : accProbs)
            Nothing -> case labelOf src True stmt of
                Just lbl -> ((lbl, ExportedDef lbl stmt isOverride i) : accEntries, accProbs)
                Nothing -> (accEntries, accProbs)
        _ -> (accEntries, accProbs)

    grouped :: Map.Map String (NonEmpty Candidate)
    grouped =
        Map.fromListWith
            (<>)
            [(lbl, Candidate "" def (classOf $ edIsOverride def) (0, edOrder def) :| []) | (lbl, def) <- entries]

    collect cands (defs, probs) =
        let (winner, losers) = resolvePriority cands
         in (candDef winner : defs, probs ++ map dupWarning losers)

    dupWarning c = mkProblem Warning DuplicateDirective (getSpan (edStmt (candDef c)))

{- | Resolve a file's effective scope: local statements plus each dependency's
exported scope, picking exactly one definition per label by priority (override
beats plain; among equal precedence, earliest position wins).

Note on cross-file order: there's no single lexical order spanning multiple files,
so this implementation ranks local candidates ahead of imported ones at equal
precedence, and ranks imported candidates by the position of the @using@ directive
that introduced them, then by their own position in the origin file.

An imported definition's precedence class depends only on whether it was pulled in
via an overriding @using@ — a definition's own override status in its origin file
only affects resolution *within that file* (i.e. which definition becomes its
exported entry), and isn't itself carried across the @using@ boundary.
-}
effectiveScope ::
    FilePath ->
    [ExportedDef] ->
    {- | (dependency path, was pulled in via an overriding @using@, dependency's
    exported scope), one entry per @using@ directive, in lexical order.
    -}
    [(FilePath, Bool, Map.Map String ExportedDef)] ->
    (Map.Map String ResolvedDef, [SemanticProblem])
effectiveScope self localCandidates deps = Map.foldrWithKey resolve (Map.empty, []) grouped
  where
    grouped :: Map.Map String (NonEmpty Candidate)
    grouped = Map.fromListWith (<>) (localEntries ++ importedEntries)

    localEntries =
        [ (edLabel d, Candidate self d (classOf $ edIsOverride d) (0, edOrder d) :| [])
        | d <- localCandidates
        ]

    importedEntries =
        [ (edLabel d, Candidate depPath d (classOf isOverrideUsing) (usingOrder + 1, edOrder d) :| [])
        | (usingOrder, (depPath, isOverrideUsing, exported)) <- zip [0 :: Int ..] deps
        , d <- Map.elems exported
        ]

    resolve lbl cands (accepted, warnings) =
        let (winner, losers) = resolvePriority cands
         in ( Map.insert lbl (ResolvedDef (candFrom winner) (candDef winner)) accepted
            , warnings ++ mapMaybe (loserWarning winner) losers
            )

    -- Suppress the warning for a losing imported-scope candidate when the winner is
    -- override-class, so an intentional override doesn't also complain about the
    -- definition it's overriding.
    loserWarning winner loser
        | candFrom loser /= self && candClass winner == 0 = Nothing
        | otherwise = Just . mkProblem Warning DuplicateLabel . getSpan . edStmt . candDef $ loser
