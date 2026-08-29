{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Options

import Betzac.Debug.Dot.Visitor (toDot)
import qualified Betzac.Pipeline as B (PipelineResult (..))

import Betzac.Compilation.Context (CompilationContext (..), FileBody (Compiled), FileEntry (..), feDiagnostics, feHasError, fePipeline)
import Betzac.Compilation.Driver (discover, resolvePrelude, resolveScopes, sealPrelude)
import Betzac.Compilation.Driver.Store (defaultStore, publishInterfaces)
import Betzac.Compilation.Flag (CompilerOptions (terminateOnFirstError), applyOptions, optionsFromFlags)
import Betzac.Debug.PrettyPrint (PrettyPrint (..))
import Betzac.Diagnostic (SemanticProblem (..), SemanticProblemKind (CompilationSucceeded), Severity (Error), causeOf)
import Betzac.Located (Located (tokenVal))

import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
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
    Just (ts, mbundle) ->
        return
            (maybe ok (const $ err $ mempty) mbundle)
                { stageDetail = do
                    mapM_ (hPutBundlePretty S.stderr) mbundle
                    mapM_ (hPutIndentLn S.stderr . show . tokenVal) ts
                    S.hPutStrLn S.stderr ""
                }

showParseResults :: Options -> B.PipelineResult -> IO StageResult
showParseResults o p = case B.parseResult p of
    Nothing -> return notRun
    Just (program, mbundle) -> do
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
        return
            (maybe ok (const $ err mempty) mbundle)
                { stageDetail = mapM_ (hPutBundlePretty S.stderr) mbundle >> dotNote
                }

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

{- | Summarize the whole discovered dependency tree: every file's own lex/parse/
analysis outcome (each file in the tree had to be lexed and parsed to discover it
in the first place, not just the target) plus its directive-level diagnostics, and
the target's resolved effective scope.
-}
showDependencies :: CompilationContext -> IO StageResult
showDependencies ctx = do
    let entries = Map.toList (ccFiles ctx)
        hasError = any (feHasError . snd) entries
        detail = do
            mapM_ (hPutIndentLn S.stderr . describeFile) entries
            case feEffective =<< Map.lookup (ccTarget ctx) (ccFiles ctx) of
                Just eff -> hPutIndentLn S.stderr ("Target effective scope labels: " ++ show (Map.keys eff))
                Nothing -> mempty
    return $ if hasError then err detail else ok{stageDetail = detail}
  where
    describeFile (path, entry) =
        path
            ++ " ["
            ++ show (feStatus entry)
            ++ "]"
            ++ " lex="
            ++ resultTag (B.lexResult =<< fePipeline entry)
            ++ " parse="
            ++ resultTag (B.parseResult =<< fePipeline entry)
            ++ " analysis="
            ++ maybe passed (\ps -> if any ((== Error) . semSev) ps then failure else success) (B.semanticResult =<< fePipeline entry)
            ++ concatMap ((" " ++) . describeDiag) (feDiagnostics entry)

    describeDiag d = "(" ++ show (semSev d) ++ " " ++ causeOf (semKind d) ++ ")"

    resultTag Nothing = passed
    resultTag (Just (_, Just _)) = failure
    resultTag (Just (_, Nothing)) = success

data StageResult = StageResult
    { stageStatus :: String
    , stageDetail :: IO ()
    , stageFailed :: Bool
    }

ok, notRun :: StageResult
ok = StageResult success mempty False
notRun = StageResult passed mempty False

err :: IO () -> StageResult
err details = StageResult failure details True

success, failure, passed :: String
success = "Ok"
failure = "Fail"
passed = "(not run)"

{- | Run one stage, print its status/detail per verbosity, and report whether it
failed. Unlike the old behaviour, this no longer exits the process itself — only a
@system@-cause failure or, with @-Wfatal-errors@, the first failing stage does that
(cf. 'runStages').
-}
hPutStage :: Options -> String -> IO StageResult -> IO Bool
hPutStage o lbl action = do
    StageResult status detail failed <- action
    when (verbosity o >= Verbose) $
        S.hPutStrLn S.stderr (lbl ++ ": " ++ status)
    whenVeryVerbose o detail
    pure failed

{- | Run a sequence of labelled stages, collecting whether any of them failed.
Without @-Wfatal-errors@, every stage runs regardless of earlier failures (so all
diagnostics across the whole target get reported, per 2.1's spirit of not stopping
prematurely); with it, stop at the first failing stage.
-}
runStages :: Options -> CompilerOptions -> [(String, IO StageResult)] -> IO Bool
runStages opts copts = go False
  where
    go anyFailed [] = pure anyFailed
    go anyFailed ((lbl, action) : rest) = do
        failed <- hPutStage opts lbl action
        let anyFailed' = anyFailed || failed
        if failed && terminateOnFirstError copts
            then pure anyFailed'
            else go anyFailed' rest

{- | Apply the compiler's warning-flag configuration to every discovered file's
diagnostics, both the directive-level ones and each file's own semantic-pass
results, before anything gets displayed.
-}
applyOptionsToContext :: CompilerOptions -> CompilationContext -> CompilationContext
applyOptionsToContext copts ctx = ctx{ccFiles = Map.map adjustEntry (ccFiles ctx)}
  where
    adjustEntry entry =
        entry
            { feDirectiveProblems = applyOptions copts (feDirectiveProblems entry)
            , feScopeProblems = applyOptions copts (feScopeProblems entry)
            , feBody = adjustBody (feBody entry)
            }
    adjustBody (Compiled pr) = Compiled pr{B.semanticResult = fmap (applyOptions copts) (B.semanticResult pr)}
    adjustBody body = body

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
        copts = optionsFromFlags (compilerFlags opts)
    -- Discovery lexes and parses the whole `using` dependency tree (not just the
    -- target) to even find out what that tree is, so it runs before, and subsumes,
    -- the target's own single-file LEXER/PARSER/ANALYSIS stages below.
    store <- defaultStore
    result <- runExceptT $ do
        prelude <- ExceptT $ resolvePrelude (preludePath opts)
        ctx0 <- ExceptT $ discover T.readFile (Just store) root fp (Just prelude) copts
        ExceptT $ return $ sealPrelude $ resolveScopes ctx0
    case result of
        -- A `system`-cause failure always terminates immediately, regardless of flags.
        Left problem -> do
            S.hPutStrLn S.stderr $ "Fatal: " ++ causeOf (semKind problem)
            S.exitFailure
        Right ctx1 -> do
            -- Before warning flags are applied: what is cacheable must not depend on them.
            publishInterfaces store ctx1
            let ctx = applyOptionsToContext copts ctx1
            case Map.lookup (ccTarget ctx) (ccFiles ctx) of
                Nothing -> do
                    S.hPutStrLn S.stderr "Fatal: target file missing from discovered context"
                    S.exitFailure
                Just targetEntry | Just results <- fePipeline targetEntry -> do
                    whenVeryVerbose opts $
                        T.hPutStrLn S.stderr $
                            (T.unlines . map ("    " <>) . T.lines) (B.sourceText results)
                    anyFailed <-
                        runStages
                            opts
                            copts
                            [ ("LEXER", showLexResults opts results)
                            , ("PARSER", showParseResults opts results)
                            , ("ANALYSIS", showAnalysis opts results)
                            , ("DEPENDENCIES", showDependencies ctx)
                            ]
                    if anyFailed
                        then S.exitFailure
                        else S.hPutStrLn S.stderr ("[Info] " ++ causeOf CompilationSucceeded)
                -- The target is always compiled, never served from its own interface.
                Just _ -> do
                    S.hPutStrLn S.stderr "Fatal: target file was not compiled"
                    S.exitFailure
