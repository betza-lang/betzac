module Betzac.Compilation.Label (checkLabels) where

import Betzac.AST.Utils
import Betzac.Compilation.Context
import Betzac.Compilation.Label.Liveness (checkDeadLabels)
import Betzac.Compilation.Label.Resolution (resolveLabelBody)
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic

import qualified Data.Map as Map

{- | Unresolved/circular references first; only if that comes back clean does dead-label
detection run at all. Its own 'stage' call is never even forced otherwise.
-}
checkLabels :: LabelTable ResolvedDef -> LabelTable ExportedDef -> FilePath -> [SemanticProblem]
checkLabels eff exported file = runStage_ $ do
    stage () correctnessProbs
    stage () (checkDeadLabels eff exported file)
  where
    locals = [(name, def) | (name, ResolvedDef from def) <- Map.toList eff, from == file]
    correctnessProbs = concatMap (\(name, def) -> resolveLabelBody eff file name (exprOf $ edStmt def)) locals
