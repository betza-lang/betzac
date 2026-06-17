{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Betzac.Debug.Dot.Visitor (toDot)
import Betzac.Lexer.Core (LexError (..))
import Betzac.Parser.Core (ParseError (..))
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Control.Exception (IOException, try)
import Control.Monad (when)
import qualified Data.Text as T (Text, lines, unlines)
import qualified Data.Text.IO as T
import Options
import qualified System.Exit as S
import qualified System.IO as S
import Prelude hiding (readFile)

showLexResults :: Options -> PipelineResult -> IO StageResult
showLexResults _ p = case lexResult p of
    Nothing -> return notRun
    Just (Left (LexError pos)) -> do
        hPutIndentLn S.stderr $ "Lex error at position " ++ show pos
        return $ err mempty
    Just (Right tokens) -> do
        return
            ok
                { stageDetail = do
                    mapM_ (hPutIndentLn S.stderr . show) tokens
                    S.hPutStrLn S.stderr ""
                }

showParseResults :: Options -> PipelineResult -> IO StageResult
showParseResults o p = case parseResult p of
    Nothing -> return notRun
    Just (Left ParseError) -> do
        hPutIndentLn S.stderr "Parse error"
        return $ err mempty
    Just (Right program) -> do
        case emitDot o of
            Just "/dev/null" -> mempty
            Just "stdout" -> T.hPutStr S.stdout (toDot program)
            Just "-" -> T.hPutStr S.stdout (toDot program)
            Just path -> T.writeFile path (toDot program)
            Nothing -> mempty
        let dotNote = case emitDot o of
                Just path | path `notElem` ["/dev/null", "stdout", "-"] -> hPutIndentLn S.stderr ("Wrote AST to " ++ path)
                _ -> mempty
        return ok{stageDetail = dotNote}

showAnalysis :: Options -> PipelineResult -> IO StageResult
showAnalysis _ _ = return notRun

data StageResult = StageResult
    { stageStatus :: String
    , stageDetail :: IO ()
    , stageExit :: IO ()
    }

ok, notRun :: StageResult
ok = StageResult "Ok" mempty mempty
notRun = StageResult "(not run)" mempty mempty
err :: IO () -> StageResult
err details = StageResult "Fail" details S.exitFailure

success, failure, passed :: String
success = "Ok"
failure = "Fail"
passed = "(not run)"

hPutStage :: Options -> String -> IO StageResult -> IO ()
hPutStage o label action = do
    StageResult status detail exit <- action
    when (verbosity o >= Verbose) $
        S.hPutStrLn S.stderr (label ++ ": " ++ status)
    whenVeryVerbose o detail
    exit

-- | Run a detail action only if verbosity >= -vv
whenVeryVerbose :: Options -> IO () -> IO ()
whenVeryVerbose o action = if verbosity o >= VeryVerbose then action else mempty

hPutIndentLn :: S.Handle -> String -> IO ()
hPutIndentLn h s = S.hPutStrLn h $ "    " ++ s

main :: IO ()
main = do
    opts <- getOptions
    f <- try (T.readFile $ inputFile opts) :: IO (Either IOException T.Text)
    case f of
        Left e -> do
            when (verbosity opts >= Verbose) $ S.hPutStrLn S.stderr ("IO: " ++ failure)
            hPutIndentLn S.stderr $ "Could not read file: " ++ show e
            S.exitFailure
        Right src -> do
            when (verbosity opts >= Verbose) $ S.hPutStrLn S.stderr ("IO: " ++ success)
            whenVeryVerbose opts $
                T.hPutStrLn S.stderr $
                    (T.unlines . map ("    " <>) . T.lines) src
            case fromScratch src of
                Left e -> do
                    S.hPutStrLn S.stderr $ "Fatal pipeline error: " ++ show e
                    S.exitFailure
                Right results -> do
                    hPutStage opts "LEXER" $ showLexResults opts results
                    hPutStage opts "PARSER" $ showParseResults opts results
                    hPutStage opts "ANALYSIS" $ showAnalysis opts results
