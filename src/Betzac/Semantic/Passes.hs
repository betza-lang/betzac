module Betzac.Semantic.Passes (runAllPasses) where

import Betzac.AST
import Betzac.Located (Span (Generated))
import Betzac.Semantic.Core

runAllPasses :: BetzaProgram Ps -> [SemanticProblem]
runAllPasses _ = runPass $ do
    -- checkAmalgamatedDirections ast
    -- checkAllInFirstLeg ast
    -- etc.
    emitWarningAt Unknown (Descriptor "UNIMPLEMENTED" $ PsX Generated :: Label Ps)
