{- | Entry point for multi-file compilation: discover a target's @using@ dependency
graph, then resolve every discovered file's scope.
-}
module Betzac.Compilation.Driver (SourceReader, discover, resolveScopes) where

import Betzac.Compilation.Driver.Discovery (SourceReader, discover)
import Betzac.Compilation.Driver.Resolve (resolveScopes)
