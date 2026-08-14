{-# LANGUAGE OverloadedStrings #-}

module Betzac.Compilation.Label.Scope (
    LabelTable,
    Candidate (..),
    labelText,
    exportedScope,
    localDefs,
    effectiveScope,
    checkLabelRefs,
) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (
    BetzaProgram,
    BetzaStmt (..),
    Directive (..),
    Label (..),
    Labelling,
    QualifiedStmt (..),
 )
import Betzac.Compilation.Context (ExportedDef (..), ResolvedDef (..))
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel, UnusedLabel),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Span (HasSpan (..))

-- Note: this module resolves which definition wins per label. It does not yet track
-- whether a label/using target is ever referenced, so unused-label and unused-file
-- detection are not implemented here — planned as follow-up work.

type LabelTable a = Map.Map Labelling a

-- | Normalized text of a label, used as the key into scope maps.
labelText :: Label p -> Labelling
labelText (Upper c _) = [c]
labelText (Descriptor s _) = s
labelText (Leaper m n _) = show m ++ "," ++ show n

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
bareLabelRef (LabelRef lbl _) = Just lbl
bareLabelRef _ = Nothing

{- | The label a statement defines, if any. A bare label-resolving reference never
defines anything (see 'bareLabelRef') — it only points at a label defined elsewhere.
-}
labelOf :: BetzaStmt Ps -> Maybe String
labelOf (Assign lbl _ _) = Just $ labelText lbl
labelOf (LabelRef _ _) = Nothing

{- | All Assign-defined labels within a single file, independent of whether they are
exported — the candidate pool for that file's own local scope.
-}
localDefs :: BetzaProgram Ps -> [ExportedDef]
localDefs prog = mapMaybe (uncurry build) (zip [0 ..] prog)
  where
    build i qs = do
        (stmt, isOverride, _) <- statementOf qs
        lbl <- labelOf stmt
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

{- | The labels a file exposes to other files via export. A same-label export repeated
within one file yields only the highest-priority definition; the rest are ignored
and warned as a duplicate directive. A bare label-resolving export (@export A;@)
exposes whatever wins for that label in the file's effective scope — local or
imported via @using@ — rather than exporting the reference statement itself; if the
label doesn't resolve at all, it's an unresolved label.
-}
exportedScope :: LabelTable ResolvedDef -> BetzaProgram Ps -> ([ExportedDef], [SemanticProblem])
exportedScope eff prog =
    let (defs, dupProbs) = Map.foldr collect ([], []) grouped
     in (defs, dupProbs ++ reverse refProbs)
  where
    (entries, refProbs) = foldl' step ([], []) (zip [0 :: Int ..] prog)

    step (accEntries, accProbs) (i, qs) = case statementOf qs of
        Just (stmt, isOverride, True) -> case bareLabelRef stmt of
            Just lbl -> case Map.lookup (labelText lbl) eff of
                Just (ResolvedDef _ def) -> ((edLabel def, def) : accEntries, accProbs)
                Nothing -> (accEntries, mkProblem Error (UnresolvedLabel) (getSpan stmt) : accProbs)
            Nothing -> case labelOf stmt of
                Just lbl -> ((lbl, ExportedDef lbl stmt isOverride i) : accEntries, accProbs)
                Nothing -> (accEntries, accProbs)
        _ -> (accEntries, accProbs)

    grouped :: LabelTable (NonEmpty Candidate)
    grouped =
        Map.fromListWith
            (<>)
            [(lbl, Candidate "" def (classOf $ edIsOverride def) (0, edOrder def) :| []) | (lbl, def) <- entries]

    collect cands (defs, probs) =
        let (winner, losers) = resolvePriority cands
         in (candDef winner : defs, probs ++ map dupWarning losers)

    dupWarning c = mkProblem Warning DuplicateDirective (getSpan (edStmt (candDef c)))

{- | Diagnostics for every *unexported* bare label-resolving reference statement in a
file (@label;@ with no @export@): checked against the same effective scope as any
other reference, so a label that resolves nowhere is 'UnresolvedLabel' just as it
would be from inside any other statement's body. Referencing a label from an
unexported, unlabelled statement has no observable effect on the rest of the file, so
one that does resolve is always reported 'UnusedLabel' instead. An exported bare
reference never reaches here — 'exportedScope' re-exports the local winner for that
label rather than treating the reference statement itself as a target of these checks.
-}
checkLabelRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkLabelRefs eff prog =
    [ problem
    | qs <- prog
    , Just (stmt, _, False) <- [statementOf qs]
    , Just lbl <- [bareLabelRef stmt]
    , let problem = case Map.lookup (labelText lbl) eff of
            Nothing -> mkProblem Error UnresolvedLabel (getSpan stmt)
            Just _ -> mkProblem Warning (UnusedLabel (labelText lbl)) (getSpan stmt)
    ]

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
    [(FilePath, Bool, LabelTable ExportedDef)] ->
    (LabelTable ResolvedDef, [SemanticProblem])
effectiveScope self localCandidates deps = Map.foldrWithKey resolve (Map.empty, []) grouped
  where
    grouped :: LabelTable (NonEmpty Candidate)
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
