{- | Entry point for multi-file compilation: locate the standard prelude, discover a
target's @using@ dependency graph, then resolve every discovered file's scope.
-}
module Betzac.Compilation.Driver (SourceReader, discover, resolveScopes, resolvePrelude, sealPrelude) where

import Betzac.Compilation.Driver.Discovery (SourceReader, discover)
import Betzac.Compilation.Driver.Prelude (resolvePrelude, sealPrelude)
import Betzac.Compilation.Driver.Resolve (resolveScopes)
