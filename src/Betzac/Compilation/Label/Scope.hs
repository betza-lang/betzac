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
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

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
import Betzac.Span (HasSpan (..), Span)

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

{- | The label a statement is *about* — the one it defines ('Assign') or the one it
merely references ('LabelRef'). Unlike 'labelOf', this never returns 'Nothing': every
statement names some label, whether it's defining fresh content for it or just
pointing at whatever already resolves for it elsewhere.
-}
stmtLabel :: BetzaStmt p -> Label p
stmtLabel (Assign lbl _ _) = lbl
stmtLabel (LabelRef lbl _) = lbl

{- | All Assign-defined labels within a single file, independent of whether they are
exported — the candidate pool for that file's own local scope.
-}
localDefs :: BetzaProgram Ps -> [ExportedDef]
localDefs prog = mapMaybe (uncurry build) (zip [0 ..] prog)
  where
    build i qs = do
        (stmt, isOverride, _) <- statementOf qs
        lbl <- labelOf stmt
        return $ ExportedDef lbl qs isOverride i

{- | One candidate definition contending for a label, tagged with its origin file, its
precedence class (0 = override, highest priority — see 'classOf'; only used to gate
the imported-loser suppression in 'effectiveScope''s @loserWarning@, per spec
2.4.26.1), its full priority order (see 'effectiveScope''s @rankOf@) for picking the
winner among all candidates, and where a diagnostic about it losing should point.
-}
data Candidate = Candidate
    { candFrom :: FilePath
    , candDef :: ExportedDef
    , candClass :: Int
    , candOrder :: (Int, Int, Int)
    , candDiagSpan :: Span
    {- ^ Where to anchor a diagnostic about this candidate, in a file the current
    resolution can actually attribute it to: the candidate's own span if it's local,
    or the @using@ directive that pulled it in if it's imported — never a foreign
    file's internal coordinates, which no diagnostic here is ever entitled to point
    into.
    -}
    }

classOf :: Bool -> Int
classOf isOverride = if isOverride then 0 else 1

{- | Pick the winning candidate for a label by priority ('candOrder' — see
'effectiveScope''s @rankOf@ for what it encodes); among equal order, earliest
position wins.
-}
resolvePriority :: NonEmpty Candidate -> (Candidate, [Candidate])
resolvePriority cands = case sortOn (\c -> (candClass c, candOrder c)) (foldr (:) [] cands) of
    (w : ls) -> (w, ls)
    [] -> error "resolvePriority: unreachable, NonEmpty is never empty"

{- | The labels a file exposes to other files via export. @export label = expr;@ is
equivalent to @export label; label = expr;@ — the exporting statement's own
@override@/precedence only affects *which definition wins* for that label (via
'effectiveScope', same as any other local definition); the export directive itself
just republishes whatever wins for that label in the file's effective scope, local or
imported via @using@, never the exporting statement's own body directly. If the label
doesn't resolve at all, it's an unresolved label. A same-label export repeated within
one file publishes only once — the earliest occurrence — and warns every later one as
a duplicate directive.
-}
exportedScope :: LabelTable ResolvedDef -> BetzaProgram Ps -> ([ExportedDef], [SemanticProblem])
exportedScope eff prog =
    let (defs, dupProbs) = Map.foldr collect ([], []) grouped
     in (defs, dupProbs ++ reverse refProbs)
  where
    (entries, refProbs) = foldl' step ([], []) (zip [0 :: Int ..] prog)

    step (accEntries, accProbs) (i, qs) = case statementOf qs of
        Just (stmt, _, True) -> case Map.lookup (labelText (stmtLabel stmt)) eff of
            Just (ResolvedDef _ def) -> ((edLabel def, (i, qs, def)) : accEntries, accProbs)
            Nothing -> (accEntries, mkProblem Error UnresolvedLabel (getSpan stmt) : accProbs)
        _ -> (accEntries, accProbs)

    grouped :: LabelTable (NonEmpty (Int, QualifiedStmt Ps, ExportedDef))
    grouped = Map.fromListWith (<>) [(lbl, occ :| []) | (lbl, occ) <- entries]

    collect occurrences (defs, probs) =
        case NE.sortBy (comparing (\(i, _, _) -> i)) occurrences of
            (_, _, def) :| rest -> (def : defs, probs ++ map dupWarning rest)

    dupWarning (_, qs, _) = mkProblem Warning DuplicateDirective (getSpan qs)

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
exported scope, picking exactly one definition per label by priority — four ranks,
strongest first:

  0. Local @override@: the most specific, most deliberate act, physically at the
     point of conflict — it wins over everything, including an @override using@.
  1. Imported via @override using@: the tool for resolving conflicts *among*
     imports (cf. spec 2.4.23), not for beating a local, deliberate override.
  2. Imported via a plain @using@: already-published, externally-owned content, so
     it outranks a same-named plain local definition by default — @override@ is the
     explicit way to say "yes, I meant to replace the imported one."
  3. Local, plain: weakest — the thing that gets shadowed by default.

Within any rank shared by more than one candidate (multiple local overrides of the
same label; multiple @using@s pulling in the same label at the same rank), the
earliest one in lexical order wins — for imports, ordered by the position of the
@using@ directive that introduced them, then by their own position in the origin
file.
-}
effectiveScope ::
    FilePath ->
    [ExportedDef] ->
    {- | (dependency path, was pulled in via an overriding @using@, span of the
    @using@ directive itself, dependency's exported scope), one entry per @using@
    directive, in lexical order.
    -}
    [(FilePath, Bool, Span, LabelTable ExportedDef)] ->
    (LabelTable ResolvedDef, [SemanticProblem])
effectiveScope self localCandidates deps = Map.foldrWithKey resolve (Map.empty, []) grouped
  where
    grouped :: LabelTable (NonEmpty Candidate)
    grouped = Map.fromListWith (<>) (localEntries ++ importedEntries)

    -- The four ranks above, folding override-vs-plain and local-vs-imported into one
    -- ordering in a single place, rather than two independently-reasoned-about axes.
    rankOf isLocal isOverride = case (isLocal, isOverride) of
        (True, True) -> 0 :: Int
        (False, True) -> 1
        (False, False) -> 2
        (True, False) -> 3

    localEntries =
        [ (edLabel d, Candidate self d (classOf $ edIsOverride d) (rankOf True (edIsOverride d), 0, edOrder d) (getSpan d) :| [])
        | d <- localCandidates
        ]

    importedEntries =
        [ (edLabel d, Candidate depPath d (classOf isOverrideUsing) (rankOf False isOverrideUsing, usingOrder, edOrder d) usingSpan :| [])
        | (usingOrder, (depPath, isOverrideUsing, usingSpan, exported)) <- zip [0 :: Int ..] deps
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
        | otherwise = Just . mkProblem Warning DuplicateLabel . candDiagSpan $ loser
