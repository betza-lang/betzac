{- | Entry point for multi-file compilation: locate the standard prelude, discover a
target's @using@ dependency graph, then resolve every discovered file's scope.
-}
module Betzac.Compilation.Driver (SourceReader, SourceCompiler, SourceAccess (..), discover, discoverWith, resolveScopes, resolvePrelude, sealPrelude) where

import Betzac.Compilation.Driver.Discovery (SourceAccess (..), SourceCompiler, SourceReader, discover, discoverWith)
import Betzac.Compilation.Driver.Prelude (resolvePrelude, sealPrelude)
import Betzac.Compilation.Driver.Resolve (resolveScopes)
