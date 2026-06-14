module Main (main) where

import Betzac.Lexer.Core (LexError (..))
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Control.Exception (IOException, try)
import Data.Text (Text)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (readFile)

showLexResults :: PipelineResult -> IO ()
showLexResults p = case lexResult p of
    Nothing -> mempty
    Just (Left (LexError pos)) -> do
        hPutStrLn stderr $ "Lex error at position " ++ show pos
        exitFailure
    Just (Right tokens) -> mapM_ print tokens

showParseResults :: PipelineResult -> IO ()
showParseResults = mempty

showAnalysis :: PipelineResult -> IO ()
showAnalysis = mempty

main :: IO ()
main = do
    args <- getArgs
    case args of
        [path] -> do
            f <- try (readFile path) :: IO (Either IOException Text)
            case f of
                Left err -> do
                    hPutStrLn stderr $ "Could not read file"
                    hPutStrLn stderr $ show err
                    exitFailure
                Right src -> case fromScratch src of
                    Left err -> hPutStrLn stderr $ "Fatal pipeline error: " ++ show err
                    Right results -> do
                        showLexResults results
                        showParseResults results
                        showAnalysis results
        _ -> do
            hPutStrLn stderr "Usage: betzac <file>"
            exitFailure
