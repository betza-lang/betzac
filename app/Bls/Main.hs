module Main (main) where

import System.Environment (getArgs)

import Betzac.Version (versionString)
import LangServer.Server (serverBLS)
import Language.LSP.Server (runServer)

main :: IO Int
main = do
    args <- getArgs
    if any isVersionFlag args
        then 0 <$ putStrLn (versionString "bls")
        else runServer serverBLS

isVersionFlag :: String -> Bool
isVersionFlag arg = arg `elem` ["--version", "-V"]
