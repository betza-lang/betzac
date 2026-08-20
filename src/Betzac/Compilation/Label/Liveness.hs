module Betzac.Compilation.Label.Liveness (checkDeadLabels, checkDeadRefs) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram, Labelling)
import Betzac.AST.Utils (exprLabels)
import Betzac.Compilation.Context
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic
import Betzac.Span

import qualified Data.Map as Map
import qualified Data.Set as Set

checkDeadLabels :: LabelTable ResolvedDef -> LabelTable ExportedDef -> FilePath -> [SemanticProblem]
checkDeadLabels eff exported file =
    [ mkProblem Warning (UnusedLabel name) (getSpan def)
    | (name, ResolvedDef from def) <- Map.toList eff
    , from == file
    , not $ name `Set.member` live
    ]
  where
    roots = Map.keysSet exported -- exported names are live by definition
    live = foldl' reach roots $ Map.elems exported

    reach seen def = foldl' visit seen $ refsOf def
    visit seen name
        | name `Set.member` seen = seen
        | otherwise = case Map.lookup name eff of
            Just (ResolvedDef from def) | from == file -> reach (Set.insert name seen) def
            _ -> seen -- undefined, or imported and so never dead here
    refsOf :: ExportedDef -> [Labelling]
    refsOf = foldMap (map labelText . exprLabels) . edExpr

{- | A bare label reference that resolves, but is neither exported nor bound to
anything: naming a label on its own achieves nothing.
-}
checkDeadRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkDeadRefs eff prog =
    [ mkProblem Warning (UnusedLabel name) (getSpan stmt)
    | (stmt, name) <- unexportedLabelRefs prog
    , Map.member name eff
    ]
