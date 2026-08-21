module Main (main) where

import System.Environment (getArgs)

import Betzac.Version (versionString)
import LangServer.Server (serverBLS)
import Language.LSP.Server (runServer)

main :: IO Int
main = do
    args <- getArgs
    if isVersionRequest args
        then 0 <$ putStrLn (versionString "bls")
        else runServer serverBLS

-- | Reporting the version is its own mode: accepted alone, and nowhere else.
isVersionRequest :: [String] -> Bool
isVersionRequest args = args == ["--version"]
