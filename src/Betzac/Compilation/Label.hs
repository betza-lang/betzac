module Betzac.Compilation.Label (checkLabels) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram)
import Betzac.AST.Utils
import Betzac.Compilation.Context
import Betzac.Compilation.Label.Liveness (checkDeadLabels)
import Betzac.Compilation.Label.Resolution (resolveLabelBody)
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic

import qualified Data.Map as Map

{- | Unresolved/circular references first; only if that comes back clean does dead-label
detection run at all. Its own 'stage' call is never even forced otherwise. Bare
label-reference statements (cf. 'checkLabelRefs') are an independent source of
diagnostics — logged alongside, never gating the rest of the chain.
-}
checkLabels :: LabelTable ResolvedDef -> LabelTable ExportedDef -> FilePath -> BetzaProgram Ps -> [SemanticProblem]
checkLabels eff exported file prog = runStage_ $ do
    stage () correctnessProbs
    stage () (checkDeadLabels eff exported file)
    logProblems (checkLabelRefs eff prog)
  where
    locals = [(name, def) | (name, ResolvedDef from def) <- Map.toList eff, from == file]
    correctnessProbs = concatMap (\(name, def) -> maybe [] (resolveLabelBody eff file name) (exprOf $ edStmt def)) locals
