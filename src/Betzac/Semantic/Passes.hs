module Betzac.Semantic.Passes (runAllPasses) where

import Betzac.AST
import Betzac.Semantic.Core
import Betzac.Semantic.Direction.AmalgamatedDirection (analysisAmalgamatedDirection)

checkAmalgamatedDirections :: BetzaProgram Ps -> Pass ()
checkAmalgamatedDirections prog =
    mapM_ analysisAmalgamatedDirection (universeOf prog)

runAllPasses :: BetzaProgram Ps -> [SemanticProblem]
runAllPasses ast = runPass $ do
    checkAmalgamatedDirections ast
    -- checkAllInFirstLeg ast
    -- etc.
    return ()
