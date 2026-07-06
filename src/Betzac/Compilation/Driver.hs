module Betzac.Compilation.Driver (discover, resolveScopes) where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, (</>))

import Betzac.AST.Types (Directive (Using), QualifiedStmt (Override, Plain))
import Betzac.Compilation.Context (
    CompilationContext (..),
    ExportedDef (edLabel),
    FileEntry (..),
    FileStatus (Discovered, Parsed, ScopesResolved),
    emptyContext,
 )
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Compilation.Scope (effectiveScope, exportedScope, localDefs)
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Betzac.Semantic.Core (
    SemanticProblem,
    SemanticProblemKind (SystemFailure, UsingCircular, UsingUnknown),
    Severity (Error),
    mkProblem,
 )
import Betzac.Span (Span (Generated))

{- | Discover the full dependency graph reachable from a target file's @using@
directives: resolves each path against the workspace root, and records
@using unknown@/@using circular@ diagnostics on the referencing file as they're
found, without aborting discovery of that file's other directives.

Each distinct file is read and parsed at most once (guarded by 'ccFiles'), so a file
reachable by more than one path (a diamond dependency) is only compiled once. Cycle
detection works by inserting a file's entry with status 'Discovered' *before*
descending into its own dependencies, then treating a re-encountered 'Discovered' (not
yet 'Parsed') entry as the closing edge of a cycle.

Sibling @using@ targets have no data dependency on each other during discovery (each
file's read+parse is independent); a future parallel version could fan discovery out
per unique path while keeping this cycle-detection step sequential.
-}
discover :: FilePath -> FilePath -> CompilerOptions -> IO (Either SemanticProblem CompilationContext)
discover workspaceRoot target opts = do
    rootExists <- doesDirectoryExist workspaceRoot
    if not rootExists
        then pure (Left (systemError ("workspace directory not found: " ++ workspaceRoot)))
        else do
            absRoot <- canonicalizePath workspaceRoot
            absTarget <- canonicalizePath target
            targetExists <- doesFileExist absTarget
            if not targetExists
                then pure (Left (systemError ("target file not found: " ++ absTarget)))
                else visit absTarget (emptyContext absRoot absTarget opts)
  where
    systemError msg = mkProblem Error (SystemFailure msg) Generated

{- | Read, parse, and record one file, then recurse into its @using@ targets. A read
failure (the file existed a moment ago per its caller's existence check, but couldn't
actually be read — permissions, a race, etc.) is reported as a system-level failure
rather than silently dropped.
-}
visit :: FilePath -> CompilationContext -> IO (Either SemanticProblem CompilationContext)
visit path ctx
    | Map.member path (ccFiles ctx) = pure (Right ctx)
    | otherwise = do
        readResult <- try (TIO.readFile path)
        case readResult of
            Left ioErr ->
                pure (Left (mkProblem Error (SystemFailure ("could not read " ++ path ++ ": " ++ show (ioErr :: IOException))) Generated))
            Right src -> case fromScratch path src of
                -- Unreachable with the current pipeline (it never fails structurally),
                -- but handled defensively: skip rather than fabricate a pipeline result.
                Left _ -> pure (Right ctx)
                Right pr -> do
                    let placeholder =
                            FileEntry
                                { fePipeline = pr
                                , feUsingDeps = []
                                , feOverrideUsingDeps = []
                                , feExported = Nothing
                                , feEffective = Nothing
                                , feDiagnostics = []
                                , feStatus = Discovered
                                }
                        ctx0 = ctx{ccFiles = Map.insert path placeholder (ccFiles ctx)}
                    (ctx1, deps) <- foldM (processTarget path) (ctx0, []) (rawUsingTargets pr)
                    let finalEntry =
                            placeholder
                                { feUsingDeps = map fst deps
                                , feOverrideUsingDeps = [dp | (dp, isOv) <- deps, isOv]
                                , feDiagnostics = feDiagnostics (ccFiles ctx1 Map.! path)
                                , feStatus = Parsed
                                }
                    pure (Right ctx1{ccFiles = Map.insert path finalEntry (ccFiles ctx1)})

{- | Resolve, existence-check, and (if new) recurse into one @using@ target of the
file at 'referrer', accumulating successfully-linked dependencies. Diagnostics for an
unresolvable, circular, or unreadable target are attached to 'referrer', not the
target — a broken dependency doesn't stop discovery of the referrer's other
directives.
-}
processTarget
    :: FilePath
    -> (CompilationContext, [(FilePath, Bool)])
    -> (FilePath, Bool)
    -> IO (CompilationContext, [(FilePath, Bool)])
processTarget referrer (ctx, deps) (rel, isOverride) = do
    resolved <- resolveUsingTarget (ccWorkspaceRoot ctx) rel
    case resolved of
        Nothing -> pure (addDiagnostic referrer (mkProblem Error (UsingUnknown rel) Generated) ctx, deps)
        Just path -> case Map.lookup path (ccFiles ctx) of
            Just entry
                | feStatus entry == Discovered ->
                    pure (addDiagnostic referrer (mkProblem Error (UsingCircular [referrer, path]) Generated) ctx, deps)
            _ -> do
                visited <- visit path ctx
                case visited of
                    Left problem -> pure (addDiagnostic referrer problem ctx, deps)
                    Right ctx' -> pure (ctx', deps ++ [(path, isOverride)])

-- | Valid betza source extensions, tried in order when resolving a `using` module path.
sourceExtensions :: [String]
sourceExtensions = [".betza", ".btz", ".b"]

{- | Resolve a dotted `using` module path (already dot-to-slash converted by the
parser, e.g. @my/module/test@ for @using my.module.test;@) against the workspace
root, trying each valid source extension in turn. 'Nothing' if none of them exist.
-}
resolveUsingTarget :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveUsingTarget root rel = go sourceExtensions
  where
    base = if isAbsolute rel then rel else root </> rel
    go [] = pure Nothing
    go (ext : exts) = do
        let candidate = base ++ ext
        exists <- doesFileExist candidate
        if exists then Just <$> canonicalizePath candidate else go exts

addDiagnostic :: FilePath -> SemanticProblem -> CompilationContext -> CompilationContext
addDiagnostic path diag ctx =
    ctx{ccFiles = Map.adjust (\e -> e{feDiagnostics = feDiagnostics e ++ [diag]}) path (ccFiles ctx)}

{- | The raw (unresolved), lexically-ordered @using@ targets of a parsed file, paired
with whether each came from an @override using@ directive.
-}
rawUsingTargets :: PipelineResult -> [(FilePath, Bool)]
rawUsingTargets pr = case parseResult pr of
    Just (Right prog) -> concatMap go prog
    _ -> []
  where
    go (Plain (Using fp _) _) = [(fp, False)]
    go (Override (Using fp _) _) = [(fp, True)]
    go _ = []

{- | Compute every discovered file's exported and effective scope, once discovery has
finished. Dependency edges recorded during discovery are guaranteed acyclic (a cycle
is never added to 'feUsingDeps'), so a plain per-file memoized recursion is enough to
process dependencies before dependents — no separate topological sort is needed.
-}
resolveScopes :: CompilationContext -> CompilationContext
resolveScopes ctx0 = foldl' (flip resolveFile) ctx0 (Map.keys (ccFiles ctx0))

resolveFile :: FilePath -> CompilationContext -> CompilationContext
resolveFile path ctx = case Map.lookup path (ccFiles ctx) of
    Nothing -> ctx
    Just entry -> case feEffective entry of
        Just _ -> ctx
        Nothing ->
            let ctx1 = foldl' (flip resolveFile) ctx (feUsingDeps entry)
                entry1 = ccFiles ctx1 Map.! path
                src = sourceText (fePipeline entry1)
                prog = case parseResult (fePipeline entry1) of
                    Just (Right p) -> p
                    _ -> []

                (exported, exportProbs) = exportedScope src prog
                exportedMap = Map.fromList [(edLabel d, d) | d <- exported]
                localCands = localDefs src prog

                depsInfo =
                    [ (dp, dp `elem` feOverrideUsingDeps entry1, depExported dp)
                    | dp <- feUsingDeps entry1
                    ]
                depExported dp = fromMaybe Map.empty (feExported =<< Map.lookup dp (ccFiles ctx1))

                (effective, effectiveProbs) = effectiveScope path localCands depsInfo

                entry2 =
                    entry1
                        { feExported = Just exportedMap
                        , feEffective = Just effective
                        , feDiagnostics = feDiagnostics entry1 ++ exportProbs ++ effectiveProbs
                        , feStatus = ScopesResolved
                        }
             in ctx1{ccFiles = Map.insert path entry2 (ccFiles ctx1)}
