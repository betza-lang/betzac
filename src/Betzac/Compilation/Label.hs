module Betzac.Compilation.Label (checkLabels) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram)
import Betzac.Compilation.Context
import Betzac.Compilation.Label.Liveness (checkDeadLabels, checkDeadRefs, checkUnusedImports)
import Betzac.Compilation.Label.Resolution (checkUnresolvedRefs, resolveLabelBody)
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic

import Data.Foldable (toList)
import qualified Data.Map as Map

-- | Correctness first: dead labels are only looked for once nothing is unresolved or circular.
checkLabels ::
    LabelTable ResolvedDef ->
    LabelTable ExportedDef ->
    FilePath ->
    [ImportedScope] ->
    BetzaProgram Ps ->
    [SemanticProblem]
checkLabels eff exported file imports prog = runStage_ $ do
    stage () (bodyProbs ++ checkUnresolvedRefs eff prog)
    logProblems $
        checkDeadLabels eff exported file
            ++ checkDeadRefs eff prog
            ++ checkUnusedImports eff exported file imports
  where
    bodyProbs =
        [ prob
        | (name, ResolvedDef from def) <- Map.toList eff
        , from == file
        , expr <- toList $ edExpr def
        , prob <- resolveLabelBody eff file name expr
        ]
