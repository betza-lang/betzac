{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Betzac.Debug.Dot.Visitor (toDot)
import qualified Betzac.Pipeline as B (PipelineResult (..), fromScratch)

import Control.Exception (IOException, try)
import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Options
import Text.Megaparsec hiding (failure, try)

import Betzac.Located (Located (tokenVal))
import qualified System.Exit as S
import qualified System.IO as S

showLexResults :: Options -> B.PipelineResult -> IO StageResult
showLexResults _ p = case B.lexResult p of
    Nothing -> return notRun
    Just (Left bundle) -> do
        hPutBundlePretty S.stderr bundle
        return $ err mempty
    Just (Right ts) ->
        return
            ok
                { stageDetail = do
                    mapM_ (hPutIndentLn S.stderr . show . tokenVal) ts
                    S.hPutStrLn S.stderr ""
                }

showParseResults :: Options -> B.PipelineResult -> IO StageResult
showParseResults o p = case B.parseResult p of
    Nothing -> return notRun
    Just (Left bundle) -> do
        hPutBundlePretty S.stderr bundle
        return $ err mempty
    Just (Right program) -> do
        case emitDot o of
            Just "/dev/null" -> mempty
            Just "stdout" -> T.hPutStr S.stdout (toDot program)
            Just "-" -> T.hPutStr S.stdout (toDot program)
            Just path -> T.writeFile path (toDot program)
            Nothing -> mempty
        let dotNote = case emitDot o of
                Just path
                    | path `notElem` ["/dev/null", "stdout", "-"] ->
                        hPutIndentLn S.stderr ("Wrote AST to " ++ path)
                _ -> mempty
        return ok{stageDetail = dotNote}

showAnalysis :: Options -> B.PipelineResult -> IO StageResult
showAnalysis _ _ = return notRun

data StageResult = StageResult
    { stageStatus :: String
    , stageDetail :: IO ()
    , stageExit :: IO ()
    }

ok, notRun :: StageResult
ok = StageResult success mempty mempty
notRun = StageResult passed mempty mempty

err :: IO () -> StageResult
err details = StageResult failure details S.exitFailure

success, failure, passed :: String
success = "Ok"
failure = "Fail"
passed = "(not run)"

hPutStage :: Options -> String -> IO StageResult -> IO ()
hPutStage o lbl action = do
    StageResult status detail exit <- action
    when (verbosity o >= Verbose) $
        S.hPutStrLn S.stderr (lbl ++ ": " ++ status)
    whenVeryVerbose o detail
    exit

hPutIndentLn :: S.Handle -> String -> IO ()
hPutIndentLn h s = S.hPutStrLn h $ "    " ++ s

hPutBundlePretty :: (VisualStream s, TraversableStream s, ShowErrorComponent e) => S.Handle -> ParseErrorBundle s e -> IO ()
hPutBundlePretty h bundle =
    mapM_ (hPutIndentLn h) $ lines $ errorBundlePretty bundle

-- Run a detail action only if verbosity >= -vv
whenVeryVerbose :: Options -> IO () -> IO ()
whenVeryVerbose o action = if verbosity o >= VeryVerbose then action else mempty

main :: IO ()
main = do
    opts <- getOptions
    let fp = inputFile opts
    f <- try (T.readFile fp) :: IO (Either IOException T.Text)
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
            case B.fromScratch fp src of
                Left e -> do
                    S.hPutStrLn S.stderr $ "Fatal pipeline error: " ++ show e
                    S.exitFailure
                Right results -> do
                    hPutStage opts "LEXER" $ showLexResults opts results
                    hPutStage opts "PARSER" $ showParseResults opts results
                    hPutStage opts "ANALYSIS" $ showAnalysis opts results
