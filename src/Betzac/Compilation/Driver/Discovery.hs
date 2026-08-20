module Betzac.Compilation.Driver.Discovery (SourceReader, discover) where

import Control.Exception (IOException, try)
import Control.Monad (foldM)
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist)
import System.FilePath (isAbsolute, (</>))

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (Directive (Using), QualifiedStmt)
import Betzac.AST.Utils (directiveOf, isOverride)
import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (Discovered, Parsed),
    UsingTarget (..),
    emptyContext,
 )
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, SystemFailure, UsingCircular, UsingUnknown),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Betzac.Span (HasSpan (getSpan), Span (Generated))

-- | Injected so bls can serve live editor buffers where the CLI just reads disk.
type SourceReader = FilePath -> IO Text

{- | Discover every file reachable from a target's @using@ directives. Each one is read
and parsed once, so a diamond dependency is only compiled once; a cycle shows up as a
target that is already 'Discovered' but not yet 'Parsed'.
-}
discover :: SourceReader -> FilePath -> FilePath -> CompilerOptions -> IO (Either SemanticProblem CompilationContext)
discover readSource workspaceRoot target opts = do
    rootExists <- doesDirectoryExist workspaceRoot
    if not rootExists
        then return $ Left $ systemError $ "workspace directory not found: " ++ workspaceRoot
        else do
            absRoot <- canonicalizePath workspaceRoot
            absTarget <- canonicalizePath target
            visit readSource absTarget $ emptyContext absRoot absTarget opts
  where
    systemError msg = mkProblem Error (SystemFailure msg) Generated

-- | Read, parse, and record one file, then recurse into its @using@ targets.
visit :: SourceReader -> FilePath -> CompilationContext -> IO (Either SemanticProblem CompilationContext)
visit readSource path ctx
    | Map.member path (ccFiles ctx) = return $ Right ctx
    | otherwise = do
        readResult <- try $ readSource path
        case readResult of
            Left ioErr ->
                return $ Left $ mkProblem Error (SystemFailure $ "could not read " ++ path ++ ": " ++ show (ioErr :: IOException)) Generated
            Right src -> case fromScratch path src of
                -- Unreachable with the current pipeline
                Left _ -> return $ Right ctx
                Right pr -> do
                    let placeholder =
                            FileEntry
                                { fePipeline = pr
                                , feUsingTargets = []
                                , feExported = Nothing
                                , feEffective = Nothing
                                , feDiagnostics = []
                                , feStatus = Discovered
                                }
                        ctx0 = ctx{ccFiles = Map.insert path placeholder $ ccFiles ctx}
                    (ctx1, deps) <- foldM (processTarget readSource path) (ctx0, []) $ rawUsingTargets pr
                    let (kept, dupes) = dedupeTargets deps
                        ctx2 = foldl' (flip $ addDiagnostic path) ctx1 dupes
                        finalEntry =
                            placeholder
                                { feUsingTargets = kept
                                , feDiagnostics = feDiagnostics $ ccFiles ctx2 Map.! path
                                , feStatus = Parsed
                                }
                    return $ Right ctx2{ccFiles = Map.insert path finalEntry $ ccFiles ctx2}

{- | Resolve one @using@ target and recurse into it. An unresolvable, circular, or
unreadable target is reported on 'referrer' and skipped, leaving the rest of its
directives to be discovered.
-}
processTarget ::
    SourceReader ->
    FilePath ->
    (CompilationContext, [UsingTarget]) ->
    UsingTarget ->
    IO (CompilationContext, [UsingTarget])
processTarget readSource referrer (ctx, deps) t = do
    resolved <- resolveUsingTarget (ccWorkspaceRoot ctx) (usingPath t)
    case resolved of
        Nothing -> return (addDiagnostic referrer (mkProblem Error (UsingUnknown $ usingPath t) Generated) ctx, deps)
        Just path -> case Map.lookup path $ ccFiles ctx of
            Just entry
                | feStatus entry == Discovered ->
                    return (addDiagnostic referrer (mkProblem Error (UsingCircular [referrer, path]) Generated) ctx, deps)
            _ -> do
                visited <- visit readSource path ctx
                case visited of
                    Left problem -> return (addDiagnostic referrer problem ctx, deps)
                    Right ctx' -> return (ctx', deps ++ [t{usingPath = path}])

{- | One @using@ per target file, the strongest kept so an @override using@ is never
weakened by a plain one naming the same file, plus a diagnostic for each that drops.
-}
dedupeTargets :: [UsingTarget] -> ([UsingTarget], [SemanticProblem])
dedupeTargets targets = (kept, map duplicate dropped)
  where
    (kept, dropped) = partition (\t -> Map.lookup (usingPath t) strongest == Just t) targets
    strongest = Map.fromListWith stronger [(usingPath t, t) | t <- targets]
    stronger new old = if usingIsOverride new && not (usingIsOverride old) then new else old
    duplicate t = mkProblem Warning DuplicateDirective (usingSpan t)

-- | Valid betza source extensions, tried in order when resolving a `using` module path.
sourceExtensions :: [String]
sourceExtensions = [".betza", ".btz", ".b"]

{- | Resolve a @using@ path (already dot-to-slash converted by the parser) against the
workspace root, trying each source extension in turn.
-}
resolveUsingTarget :: FilePath -> FilePath -> IO (Maybe FilePath)
resolveUsingTarget root rel = go sourceExtensions
  where
    base = if isAbsolute rel then rel else root </> rel
    go [] = return Nothing
    go (ext : exts) = do
        let candidate = base ++ ext
        exists <- doesFileExist candidate
        if exists then Just <$> canonicalizePath candidate else go exts

addDiagnostic :: FilePath -> SemanticProblem -> CompilationContext -> CompilationContext
addDiagnostic path diag ctx =
    ctx{ccFiles = Map.adjust (\e -> e{feDiagnostics = feDiagnostics e ++ [diag]}) path $ ccFiles ctx}

-- | A parsed file's @using@ targets, in lexical order, still unresolved.
rawUsingTargets :: PipelineResult -> [UsingTarget]
rawUsingTargets pr = maybe [] (mapMaybe target . fst) (parseResult pr)
  where
    target :: QualifiedStmt Ps -> Maybe UsingTarget
    target qs = case directiveOf qs of
        Using fp _ -> Just $ UsingTarget fp (isOverride qs) (getSpan qs)
        _ -> Nothing
