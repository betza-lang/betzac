module Betzac.Compilation.Label.Scope (
    LabelTable,
    ImportedScope (..),
    labelText,
    exportedScope,
    localDefs,
    effectiveScope,
    checkLabelRefs,
) where

import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram, Label (..), Labelling, QualifiedStmt)
import Betzac.AST.Utils (definedLabel, isExported, referencedLabel, stmtLabel, stmtOf)
import Betzac.Compilation.Context (
    ExportedDef (..),
    ResolvedDef (..),
    UsingTarget (..),
    edIsOverride,
 )
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel, UnusedLabel),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Span (HasSpan (..), Span)

-- TODO: flag `using` directives whose imports are never referenced

type LabelTable a = Map.Map Labelling a

-- | Normalized text of a label, used as the key into scope maps.
labelText :: Label p -> Labelling
labelText (Upper c _) = [c]
labelText (Descriptor s _) = s
labelText (Leaper m n _) = show m ++ "," ++ show n

-- | Group by key. Values within a key are unordered.
groupOn :: (Ord k) => (a -> k) -> [a] -> Map.Map k (NonEmpty a)
groupOn key xs = Map.fromListWith (<>) [(key x, pure x) | x <- xs]

-- | The strongest element by 'key', and the ones it displaces.
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

{- | Diagnostics for the unexported bare label references (@label;@) in a file. One
that resolves nowhere is unresolved; one that resolves is dead code, since referencing
a label without exporting or binding it achieves nothing.
-}
checkLabelRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkLabelRefs eff prog =
    [ if Map.member name eff
        then mkProblem Warning (UnusedLabel name) (getSpan stmt)
        else mkProblem Error UnresolvedLabel (getSpan stmt)
    | qs <- prog
    , not (isExported qs)
    , Just stmt <- [stmtOf qs]
    , Just lbl <- [referencedLabel stmt]
    , let name = labelText lbl
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
effectiveScope self locals imports = Map.foldrWithKey resolve (Map.empty, []) grouped
  where
    grouped = groupOn (edLabel . candDef) (localCandidates ++ importedCandidates)

    localPrec d = if edIsOverride d then LocalOverride else LocalPlain
    importedPrec via = if usingIsOverride via then ImportedOverride else ImportedPlain

    localCandidates = [Candidate self d (localPrec d) (0, edOrder d) (getSpan d) | d <- locals]

    importedCandidates =
        [ Candidate (usingPath via) d (importedPrec via) (order, edOrder d) (getSpan via)
        | (order, ImportedScope via defs) <- zip [0 ..] imports
        , d <- Map.elems defs
        ]

    resolve lbl cands (accepted, warnings) =
        let (winner, losers) = winnerAndLosers candKey cands
         in ( Map.insert lbl (ResolvedDef (candFrom winner) (candDef winner)) accepted
            , warnings ++ mapMaybe (loserWarning winner) losers
            )

    -- Displacing an import is exactly what an override is for, so stay quiet about it.
    loserWarning winner loser
        | isImported (candPrecedence loser) && isOverriding (candPrecedence winner) = Nothing
        | otherwise = Just $ mkProblem Warning DuplicateLabel (candDiagSpan loser)
