module Betzac.Compilation.Label.Liveness (liveLabels, checkDeadLabels, checkDeadRefs, checkUnusedImports) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram, Labelling)
import Betzac.AST.Utils (exprLabels)
import Betzac.Compilation.Context
import Betzac.Compilation.Interface (InterfaceEntry)
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic
import Betzac.Span

import qualified Data.Map as Map
import qualified Data.Set as Set

{- | Every label reachable from a file's exported scope. An imported label is marked
reached but not expanded, its body belonging to the file that defines it.
-}
liveLabels :: LabelTable ResolvedDef -> LabelTable InterfaceEntry -> FilePath -> Set.Set Labelling
liveLabels eff exported file = foldl' expand roots $ Map.keys exported
  where
    roots = Map.keysSet exported -- exported names are live by definition
    expand seen name = foldl' visit seen $ maybe [] refsOf (ownBody name)
    visit seen name
        | name `Set.member` seen = seen
        | Map.member name eff = expand (Set.insert name seen) name
        | otherwise = seen -- undefined, and reported by the resolution pass
        -- Only a definition in this file has a body to walk.
    ownBody name = do
        ResolvedDef from def <- Map.lookup name eff
        if from == file then sdExpr def else Nothing
    refsOf = map labelText . exprLabels

checkDeadLabels :: LabelTable ResolvedDef -> Set.Set Labelling -> FilePath -> [SemanticProblem]
checkDeadLabels eff live file =
    [ mkProblem Warning (UnusedLabel name) (getSpan def)
    | (name, ResolvedDef from def) <- Map.toList eff
    , from == file
    , not $ name `Set.member` live
    ]

{- | A bare label reference that resolves, but is neither exported nor bound to
anything: naming a label on its own achieves nothing.
-}
checkDeadRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkDeadRefs eff prog =
    [ mkProblem Warning (UnusedLabel name) (getSpan stmt)
    | (stmt, name) <- unexportedLabelRefs prog
    , Map.member name eff
    ]

{- | A @using@ that carries nothing into the file: either it won no label at all, or
every label it won is itself dead.
-}
checkUnusedImports :: LabelTable ResolvedDef -> Set.Set Labelling -> [ImportedScope] -> [SemanticProblem]
checkUnusedImports eff live imports =
    [ mkProblem Warning UnusedUsing (getSpan via)
    | ImportedScope via _ <- imports
    , not $ usingPath via `Set.member` contributing
    ]
  where
    contributing =
        Set.fromList [from | (name, ResolvedDef from _) <- Map.toList eff, name `Set.member` live]
