module Betzac.Compilation.Label.Scope (
    LabelTable,
    ImportedScope (..),
    labelText,
    exportedScope,
    localDefs,
    effectiveScope,
    unexportedLabelRefs,
) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram, BetzaStmt, Label (..), Labelling, QualifiedStmt)
import Betzac.AST.Utils (definedLabel, isExported, referencedLabel, stmtLabel, stmtOf)
import Betzac.Compilation.Context (
    ExportedDef (..),
    ResolvedDef (..),
    UsingTarget (..),
    edIsOverride,
 )
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnnecessaryOverride, UnresolvedLabel),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Span (HasSpan (..), Span)

type LabelTable a = Map.Map Labelling a

-- | Normalized text of a label, used as the key into scope maps.
labelText :: Label p -> Labelling
labelText (Upper c _) = [c]
labelText (Descriptor s _) = s
labelText (Leaper m n _) = show m ++ "," ++ show n

-- | Group by key. Values within a key are unordered.
groupOn :: (Ord k) => (a -> k) -> [a] -> Map.Map k (NonEmpty a)
groupOn key xs = Map.fromListWith (<>) [(key x, pure x) | x <- xs]

-- | The strongest element by 'key', and the ones it displaces, themselves in key order.
winnerAndLosers :: (Ord k) => (a -> k) -> NonEmpty a -> (a, [a])
winnerAndLosers key xs = let w :| ls = NE.sortWith key xs in (w, ls)

-- | Every label a file defines, exported or not: the pool for its own local scope.
localDefs :: BetzaProgram Ps -> [ExportedDef]
localDefs prog =
    [ ExportedDef (labelText lbl) qs i
    | (i, qs) <- zip [0 ..] prog
    , Just stmt <- [stmtOf qs]
    , Just lbl <- [definedLabel stmt]
    ]

-- | One @export@ directive, paired with what its label actually resolves to.
data ExportOccurrence = ExportOccurrence
    { occOrder :: Int
    , occStmt :: QualifiedStmt Ps
    , occDef :: ExportedDef
    }

{- | The labels a file exposes to other files. @export label = expr;@ is shorthand for
@export label; label = expr;@, so an export always republishes whatever wins for its
label in the effective scope, never its own body directly. Repeating an export of the
same label publishes it once and warns on every later occurrence.
-}
exportedScope :: LabelTable ResolvedDef -> BetzaProgram Ps -> ([ExportedDef], [SemanticProblem])
exportedScope eff prog = (winners, dupProbs ++ unresolved)
  where
    (unresolved, occurrences) =
        partitionEithers
            [ case Map.lookup (labelText (stmtLabel stmt)) eff of
                Just (ResolvedDef _ def) -> Right (ExportOccurrence i qs def)
                Nothing -> Left (mkProblem Error UnresolvedLabel (getSpan stmt))
            | (i, qs) <- zip [0 ..] prog
            , isExported qs
            , Just stmt <- [stmtOf qs]
            ]

    (winners, dupProbs) = Map.foldr collect ([], []) (groupOn (edLabel . occDef) occurrences)

    collect occs (defs, probs) =
        let (winner, losers) = winnerAndLosers occOrder occs
         in (occDef winner : defs, probs ++ map dupWarning losers)

    dupWarning = mkProblem Warning DuplicateDirective . getSpan . occStmt

{- | The bare label references (@label;@) a file makes outside an @export@, each with
the label it names. An exported one is absent: 'exportedScope' republishes it instead.
-}
unexportedLabelRefs :: BetzaProgram Ps -> [(BetzaStmt Ps, Labelling)]
unexportedLabelRefs prog =
    [ (stmt, labelText lbl)
    | qs <- prog
    , not (isExported qs)
    , Just stmt <- [stmtOf qs]
    , Just lbl <- [referencedLabel stmt]
    ]

{- | Resolution priority, strongest first: a local @override@ is the most deliberate
act at the point of conflict; @override using@ exists to settle conflicts between
imports; an import otherwise outranks a same-named local definition, which is more
often an accidental collision than a deliberate shadow. Derived 'Ord' is that order.
-}
data Precedence = LocalOverride | ImportedOverride | ImportedPlain | LocalPlain
    deriving (Eq, Ord)

isOverriding :: Precedence -> Bool
isOverriding p = p `elem` [LocalOverride, ImportedOverride]

isImported :: Precedence -> Bool
isImported p = p `elem` [ImportedOverride, ImportedPlain]

data Candidate = Candidate
    { candFrom :: FilePath
    , candDef :: ExportedDef
    , candPrecedence :: Precedence
    , candOrder :: (Int, Int)
    -- ^ Position of the @using@ that introduced it, then within its own file.
    , candDiagSpan :: Span
    -- ^ Where to report it losing, in the file being resolved.
    }

candKey :: Candidate -> (Precedence, (Int, Int))
candKey c = (candPrecedence c, candOrder c)

{- | The @using@ a candidate came through, by its position among the file's imports.
'Nothing' for a local definition.
-}
candUsingIndex :: Candidate -> Maybe Int
candUsingIndex c
    | isImported (candPrecedence c) = Just $ fst (candOrder c)
    | otherwise = Nothing

-- | The precedence a candidate would have had without its override directive.
demote :: Precedence -> Precedence
demote LocalOverride = LocalPlain
demote ImportedOverride = ImportedPlain
demote p = p

{- | Whether being an override is what put a candidate in place. Demoting it lowers it
and leaves every other candidate alone, so only the runner-up can overtake it.
-}
overrideWasNeeded :: Candidate -> [Candidate] -> Bool
overrideWasNeeded w losers = case losers of
    [] -> False
    (r : _) -> (demote (candPrecedence w), candOrder w) > candKey r

-- | Whether an @override using@ put anything in place that would not have landed there anyway.
data OverrideUse = Redundant | Promoted
    deriving (Eq, Ord)

-- | Each @override using@ by its position among the file's imports.
type OverrideUses = Map.Map Int OverrideUse

{- | An @override@ that selected nothing it would not have selected as a plain
definition. A local override is its own directive and is reported on its own statement;
an @override using@ covers everything the file it names exports, so it is reported once,
on the directive, and only when none of what it contributed needed the promotion.
-}
unnecessaryOverrides :: [ImportedScope] -> [(Candidate, [Candidate])] -> [SemanticProblem]
unnecessaryOverrides imports contests = localProbs ++ usingProbs
  where
    verdicts = [(w, overrideWasNeeded w losers) | (w, losers) <- contests]

    localProbs =
        [ mkProblem Warning UnnecessaryOverride (candDiagSpan w)
        | (w, needed) <- verdicts
        , candPrecedence w == LocalOverride
        , not needed
        ]

    uses :: OverrideUses
    uses =
        Map.fromListWith
            max
            [ (i, if needed then Promoted else Redundant)
            | (w, needed) <- verdicts
            , Just i <- [candUsingIndex w]
            ]

    usingProbs =
        [ mkProblem Warning UnnecessaryOverride (usingSpan via)
        | (i, ImportedScope via _) <- zip [0 ..] imports
        , usingIsOverride via
        , Map.findWithDefault Redundant i uses == Redundant
        ]

-- | A dependency's exported scope, and the @using@ directive that pulled it in.
data ImportedScope = ImportedScope
    { importedVia :: UsingTarget
    , importedDefs :: LabelTable ExportedDef
    }

{- | Resolve a file's effective scope: its own definitions plus everything its
dependencies export, keeping one definition per label by 'Precedence', and the
earliest among equals.
-}
effectiveScope ::
    FilePath ->
    [ExportedDef] ->
    [ImportedScope] ->
    (LabelTable ResolvedDef, [SemanticProblem])
effectiveScope self locals imports = (accepted, loserProbs ++ overrideProbs)
  where
    contests = Map.map (winnerAndLosers candKey) grouped
    grouped = groupOn (edLabel . candDef) (localCandidates ++ importedCandidates)

    localPrec d = if edIsOverride d then LocalOverride else LocalPlain
    importedPrec via = if usingIsOverride via then ImportedOverride else ImportedPlain

    localCandidates = [Candidate self d (localPrec d) (0, edOrder d) (getSpan d) | d <- locals]

    importedCandidates =
        [ Candidate (usingPath via) d (importedPrec via) (order, edOrder d) (getSpan via)
        | (order, ImportedScope via defs) <- zip [0 ..] imports
        , d <- Map.elems defs
        ]

    accepted = Map.map (\(w, _) -> ResolvedDef (candFrom w) (candDef w)) contests

    loserProbs = concat [mapMaybe (loserWarning w) losers | (w, losers) <- Map.elems contests]
    overrideProbs = unnecessaryOverrides imports (Map.elems contests)

    -- Displacing an import is exactly what an override is for, so stay quiet about it.
    loserWarning winner loser
        | isImported (candPrecedence loser) && isOverriding (candPrecedence winner) = Nothing
        | otherwise = Just $ mkProblem Warning DuplicateLabel (candDiagSpan loser)
