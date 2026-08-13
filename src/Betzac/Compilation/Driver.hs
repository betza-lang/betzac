{- | Entry point for multi-file compilation: discover a target's @using@ dependency
graph, then resolve every discovered file's scope.
-}
module Betzac.Compilation.Driver (discover, resolveScopes) where

import Betzac.Compilation.Driver.Discovery (discover)
import Betzac.Compilation.Driver.Resolve (resolveScopes)
