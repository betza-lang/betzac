module Betzac.Semantic.Passes (runAllPasses) where

import Betzac.AST.Phases
import Betzac.AST.Types
import Betzac.Semantic.Core

runAllPasses :: BetzaProgram Ps -> [SemanticProblem]
runAllPasses _ = runPass $ do
    -- checkAmalgamatedDirections ast
    -- checkAllInFirstLeg ast
    -- etc.
    return ()
