module Betzac.Semantic.Passes (runAllPasses) where

import Betzac.AST
import Betzac.AST.Utils (stmtOf)
import Betzac.Diagnostic
import Betzac.Semantic.Direction.AmalgamatedDirection (analysisAmalgamatedDirection)
import Betzac.Semantic.Label.LiteralAssignment (analysisLiteralAssignment)
import Betzac.Semantic.Modifier (analysisModifiers)
import Betzac.Semantic.Modifier.Restated (analysisRestatedModifiers)
import Data.Maybe (mapMaybe)

checkAmalgamatedDirections :: BetzaProgram Ps -> Pass ()
checkAmalgamatedDirections prog =
    mapM_ analysisAmalgamatedDirection $ universeOf prog

{- | A statement is only ever a directive's own payload, never nested inside an
expression, so the directives are the whole search -- no descent required.
-}
checkLiteralAssignment :: BetzaProgram Ps -> Pass ()
checkLiteralAssignment prog = mapM_ analysisLiteralAssignment $ mapMaybe stmtOf prog

checkModifiers :: BetzaProgram Ps -> Pass ()
checkModifiers prog = mapM_ analysisModifiers $ universeOf prog

runAllPasses :: BetzaProgram Ps -> BetzaProgram Ds -> [SemanticProblem]
runAllPasses ast desugared =
    runPass $
        checkAmalgamatedDirections ast
            >> checkLiteralAssignment ast
            >> checkModifiers ast
            >> analysisRestatedModifiers desugared
