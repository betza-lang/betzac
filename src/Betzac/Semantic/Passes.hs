module Betzac.Semantic.Passes (runAllPasses) where

import Betzac.AST
import Betzac.Diagnostic
import Betzac.Semantic.Direction.AmalgamatedDirection (analysisAmalgamatedDirection)
import Betzac.Semantic.Label.LiteralAssignment (analysisLiteralAssignment)

checkAmalgamatedDirections :: BetzaProgram Ps -> Pass ()
checkAmalgamatedDirections prog =
    mapM_ analysisAmalgamatedDirection $ universeOf prog

checkLiteralAssignment :: BetzaProgram Ps -> Pass ()
checkLiteralAssignment prog = mapM_ analysisLiteralAssignment $ universeOf prog

runAllPasses :: BetzaProgram Ps -> [SemanticProblem]
runAllPasses ast =
    runPass $
        checkAmalgamatedDirections ast
            >> checkLiteralAssignment ast
