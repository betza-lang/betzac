-- | The package version, for the executables' @--version@ flags.
module Betzac.Version (versionString) where

import Data.Version (showVersion)

import Paths_betzac (version)

-- | @name x.y.z@, as a @--version@ flag should print it.
versionString :: String -> String
versionString name = name ++ " " ++ showVersion version
