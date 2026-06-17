module Main (main) where

import Betzac.Debug.Dot.Visitor (toDot)
import Betzac.Lexer.Core (LexError (..))
import Betzac.Parser.Core (ParseError (..))
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Control.Exception (IOException, try)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T
import Options
import qualified System.Exit as S
import qualified System.IO as S
import Prelude hiding (readFile)

showLexResults :: Options -> PipelineResult -> IO ()
showLexResults _ p = case lexResult p of
    Nothing -> passed
    Just (Left (LexError pos)) -> do
        failure
        hPutIndentLn S.stderr $ "Lex error at position " ++ show pos
        S.exitFailure
    Just (Right tokens) -> success >> mapM_ (hPutIndentLn S.stderr . show) tokens

showParseResults :: Options -> PipelineResult -> IO ()
showParseResults o p = case parseResult p of
    Nothing -> passed
    Just (Left ParseError) -> failure >> hPutIndentLn S.stderr "Parse error" >> S.exitFailure
    Just (Right program) ->
        success >> case emitDot o of
            Just "/dev/null" -> mempty
            Just "stdout" -> streamProgramToStdout
            Just "-" -> streamProgramToStdout
            Just path -> writeProgramTo path >> hPutIndentLn S.stderr ("Wrote AST to " ++ path)
            Nothing -> mempty
      where
        streamProgramToStdout = T.hPutStr S.stdout (toDot program)
        writeProgramTo f = T.writeFile f (toDot program)

showAnalysis :: Options -> PipelineResult -> IO ()
showAnalysis = const . const $ S.hPutStrLn S.stderr "Not implemented yet"

hPutIndentLn :: S.Handle -> String -> IO ()
hPutIndentLn h s = S.hPutStrLn h $ "    " ++ s

failure :: IO ()
failure = S.hPutStrLn S.stderr "Fail"

passed :: IO ()
passed = S.hPutStrLn S.stderr "(not run)"

success :: IO ()
success = S.hPutStrLn S.stderr "Ok"

main :: IO ()
main = do
    opts <- getOptions
    S.hPutStr S.stderr "IO: "
    f <- try (T.readFile $ inputFile opts) :: IO (Either IOException T.Text)
    case f of
        Left err -> do
            S.hPutStrLn S.stderr $ "Could not read file"
            S.hPutStrLn S.stderr $ show err
            S.exitFailure
        Right src -> do
            success
            case fromScratch src of
                Left err -> S.hPutStrLn S.stderr $ "Fatal pipeline error: " ++ show err
                Right results -> do
                    S.hPutStr S.stderr "LEXER: "
                    showLexResults opts results
                    S.hPutStr S.stderr "PARSER: "
                    showParseResults opts results
                    S.hPutStr S.stderr "ANALYSIS: "
                    showAnalysis opts results
