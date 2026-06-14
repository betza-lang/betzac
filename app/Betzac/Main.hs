module Main (main) where

import Betzac.Lexer.Core (LexError (..))
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (readFile)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            src <- readFile path
            case lexResult (fromScratch src) of
                Nothing -> pure ()
                Just (Left (LexError pos)) -> do
                    hPutStrLn stderr $ "Lex error at position " ++ show pos
                    exitFailure
                Just (Right tokens) -> mapM_ print tokens
        _ -> do
            hPutStrLn stderr "Usage: betzac <file>"
            exitFailure
