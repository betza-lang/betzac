{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Options

import Betzac.Debug.Dot.Visitor (toDot)
import qualified Betzac.Pipeline as B (PipelineResult (..))

import Betzac.Compilation.Context (CompilationContext (..), FileEntry (..))
import Betzac.Compilation.Driver (discover, resolveScopes)
import Betzac.Compilation.Flag (optionsFromFlags)
import Betzac.Debug.PrettyPrint (PrettyPrint (..))
import Betzac.Located (Located (tokenVal))
import Betzac.Semantic.Core (SemanticProblem (..), Severity (Error), causeOf)

import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Text.Megaparsec hiding (failure, try)

import System.FilePath (takeDirectory)

import qualified System.Exit as S
import qualified System.IO as S

showLexResults :: Options -> B.PipelineResult -> IO StageResult
showLexResults _ p = case B.lexResult p of
    Nothing -> return notRun
    Just (Left bundle) -> return $ err $ hPutBundlePretty S.stderr bundle
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
    Just (Left bundle) ->
        return $ err $ hPutBundlePretty S.stderr bundle
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
showAnalysis _ p = case B.semanticResult p of
    Nothing -> return notRun
    Just problems
        | any ((== Error) . semSev) problems ->
            return $ err $ mapM_ (hPutIndentLn S.stderr . renderProblem) problems
        | otherwise ->
            return ok{stageDetail = mapM_ (hPutIndentLn S.stderr . renderProblem) problems}
  where
    renderProblem sp =
        "[" ++ show (semSev sp) ++ "] " ++ prettyPrint (semKind sp) ++ " at " ++ show (semSpan sp)

-- | Summarize the whole discovered dependency tree: every file's own lex/parse/
-- analysis outcome (each file in the tree had to be lexed and parsed to discover it
-- in the first place, not just the target) plus its directive-level diagnostics, and
-- the target's resolved effective scope.
showDependencies :: CompilationContext -> IO StageResult
showDependencies ctx = do
    let entries = Map.toList (ccFiles ctx)
        hasError = any (entryHasError . snd) entries
        detail = do
            mapM_ (hPutIndentLn S.stderr . describeFile) entries
            case feEffective =<< Map.lookup (ccTarget ctx) (ccFiles ctx) of
                Just eff -> hPutIndentLn S.stderr ("Target effective scope labels: " ++ show (Map.keys eff))
                Nothing -> mempty
    return $ if hasError then err detail else ok{stageDetail = detail}
  where
    entryHasError entry =
        any ((== Error) . semSev) (feDiagnostics entry)
            || bundleFailed (B.lexResult (fePipeline entry))
            || bundleFailed (B.parseResult (fePipeline entry))
            || maybe False (any ((== Error) . semSev)) (B.semanticResult (fePipeline entry))

    describeFile (path, entry) =
        path
            ++ " ["
            ++ show (feStatus entry)
            ++ "]"
            ++ " lex="
            ++ resultTag (B.lexResult (fePipeline entry))
            ++ " parse="
            ++ resultTag (B.parseResult (fePipeline entry))
            ++ " analysis="
            ++ maybe passed (\ps -> if any ((== Error) . semSev) ps then failure else success) (B.semanticResult (fePipeline entry))
            ++ concatMap ((" " ++) . describeDiag) (feDiagnostics entry)

    describeDiag d = "(" ++ show (semSev d) ++ " " ++ causeOf (semKind d) ++ ")"

    bundleFailed (Just (Left _)) = True
    bundleFailed _ = False

    resultTag Nothing = passed
    resultTag (Just (Left _)) = failure
    resultTag (Just (Right _)) = success

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
        root = fromMaybe (takeDirectory fp) (workspace opts)
        copts = optionsFromFlags []
    -- Discovery lexes and parses the whole `using` dependency tree (not just the
    -- target) to even find out what that tree is, so it runs before, and subsumes,
    -- the target's own single-file LEXER/PARSER/ANALYSIS stages below.
    result <- discover root fp copts
    case result of
        Left problem -> do
            S.hPutStrLn S.stderr $ "Fatal: " ++ causeOf (semKind problem)
            S.exitFailure
        Right ctx0 -> do
            let ctx = resolveScopes ctx0
            case Map.lookup (ccTarget ctx) (ccFiles ctx) of
                Nothing -> do
                    S.hPutStrLn S.stderr "Fatal: target file missing from discovered context"
                    S.exitFailure
                Just targetEntry -> do
                    let results = fePipeline targetEntry
                    whenVeryVerbose opts $
                        T.hPutStrLn S.stderr $
                            (T.unlines . map ("    " <>) . T.lines) (B.sourceText results)
                    hPutStage opts "LEXER" $ showLexResults opts results
                    hPutStage opts "PARSER" $ showParseResults opts results
                    hPutStage opts "ANALYSIS" $ showAnalysis opts results
                    hPutStage opts "DEPENDENCIES" $ showDependencies ctx
