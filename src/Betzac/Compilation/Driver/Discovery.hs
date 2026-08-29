module Betzac.Compilation.Driver.Discovery (SourceReader, SourceCompiler, SourceAccess (..), discover, discoverWith) where

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
    FileBody (Compiled),
    FileEntry (..),
    FileStatus (Discovered, Parsed),
    UsingTarget (..),
    emptyContext,
 )
import Betzac.Compilation.Driver.Store (InterfaceStore, interfacedEntry, usableInterface)
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, SystemFailure, UsingCircular, UsingUnknown),
    Severity (Error, Warning),
    mkProblem,
 )
import Betzac.Pipeline (PipelineError, PipelineResult (..), fromScratch)
import Betzac.Span (HasSpan (getSpan), Span (Generated))

-- | Injected so bls can serve live editor buffers where the CLI just reads disk.
type SourceReader = FilePath -> IO Text

-- | Injected so bls can serve an already-compiled file where the CLI compiles afresh.
type SourceCompiler = FilePath -> Text -> IO (Either PipelineError PipelineResult)

{- | How discovery obtains a file's text, its pipeline result, and its dependencies'
stored interfaces.
-}
data SourceAccess = SourceAccess
    { saRead :: SourceReader
    , saCompile :: SourceCompiler
    , saStore :: Maybe InterfaceStore
    -- ^ Where a dependency may be read from instead of compiled.
    }

-- | Reading from disk and compiling from scratch: what the CLI wants every time.
fromDisk :: SourceReader -> Maybe InterfaceStore -> SourceAccess
fromDisk readSource = SourceAccess readSource (\path src -> return $ fromScratch path src)

{- | Discover every file reachable from a target's @using@ directives, plus the standard
prelude when one is given. Each one is read and parsed once, so a diamond dependency is
only compiled once; a cycle shows up as a target that is already 'Discovered' but not
yet 'Parsed'.
-}
discover :: SourceReader -> Maybe InterfaceStore -> FilePath -> FilePath -> Maybe FilePath -> CompilerOptions -> IO (Either SemanticProblem CompilationContext)
discover readSource store = discoverWith (fromDisk readSource store)

-- | 'discover', over a caller-supplied way of reading and compiling each file.
discoverWith :: SourceAccess -> FilePath -> FilePath -> Maybe FilePath -> CompilerOptions -> IO (Either SemanticProblem CompilationContext)
discoverWith access workspaceRoot target preludePath opts = do
    rootExists <- doesDirectoryExist workspaceRoot
    if not rootExists
        then return $ Left $ systemError $ "workspace directory not found: " ++ workspaceRoot
        else do
            absRoot <- canonicalizePath workspaceRoot
            absTarget <- canonicalizePath target
            absPrelude <- traverse canonicalizePath preludePath
            let ctx0 = emptyContext absRoot absTarget absPrelude opts
            seeded <- maybe (return $ Right ctx0) (\p -> visit access (saStore access) p ctx0) absPrelude
            -- The target is always compiled: its diagnostics are the point of the run.
            either (return . Left) (visit access Nothing absTarget) seeded
  where
    systemError msg = mkProblem Error (SystemFailure msg) Generated

-- | Read, parse, and record one file, then recurse into its @using@ targets.
visit :: SourceAccess -> Maybe InterfaceStore -> FilePath -> CompilationContext -> IO (Either SemanticProblem CompilationContext)
visit access store path ctx
    | Map.member path (ccFiles ctx) = return $ Right ctx
    | otherwise = do
        cached <- maybe (return Nothing) (\s -> usableInterface s (saRead access) path) store
        case cached of
            Just i -> return $ Right ctx{ccFiles = Map.insert path (interfacedEntry i) $ ccFiles ctx}
            Nothing -> compile
  where
    compile = do
        readResult <- try $ saRead access path
        case readResult of
            Left ioErr ->
                return $ Left $ mkProblem Error (SystemFailure $ "could not read " ++ path ++ ": " ++ show (ioErr :: IOException)) Generated
            Right src -> do
                compiled <- saCompile access path src
                case compiled of
                    -- Unreachable with the current pipeline
                    Left _ -> return $ Right ctx
                    Right pr -> do
                        let placeholder =
                                FileEntry
                                    { feBody = Compiled pr
                                    , feUsingTargets = []
                                    , feExported = Nothing
                                    , feEffective = Nothing
                                    , feDirectiveProblems = []
                                    , feScopeProblems = []
                                    , feStatus = Discovered
                                    }
                            ctx0 = ctx{ccFiles = Map.insert path placeholder $ ccFiles ctx}
                        (ctx1, deps) <- foldM (processTarget access path) (ctx0, []) $ rawUsingTargets pr
                        let (kept, dupes) = dedupeTargets deps
                            ctx2 = foldl' (flip $ addDiagnostic path) ctx1 dupes
                            finalEntry =
                                placeholder
                                    { feUsingTargets = kept
                                    , feDirectiveProblems = feDirectiveProblems $ ccFiles ctx2 Map.! path
                                    , feStatus = Parsed
                                    }
                        return $ Right ctx2{ccFiles = Map.insert path finalEntry $ ccFiles ctx2}

{- | Resolve one @using@ target and recurse into it. An unresolvable, circular, or
unreadable target is reported on 'referrer' and skipped, leaving the rest of its
directives to be discovered.
-}
processTarget ::
    SourceAccess ->
    FilePath ->
    (CompilationContext, [UsingTarget]) ->
    UsingTarget ->
    IO (CompilationContext, [UsingTarget])
processTarget access referrer (ctx, deps) t = do
    resolved <- resolveUsingTarget (ccWorkspaceRoot ctx) (usingPath t)
    case resolved of
        Nothing -> return (addDiagnostic referrer (mkProblem Error (UsingUnknown $ usingPath t) Generated) ctx, deps)
        Just path -> case Map.lookup path $ ccFiles ctx of
            Just entry
                | feStatus entry == Discovered ->
                    return (addDiagnostic referrer (mkProblem Error (UsingCircular [referrer, path]) Generated) ctx, deps)
            _ -> do
                visited <- visit access (saStore access) path ctx
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
    ctx{ccFiles = Map.adjust (\e -> e{feDirectiveProblems = feDirectiveProblems e ++ [diag]}) path $ ccFiles ctx}

-- | A parsed file's @using@ targets, in lexical order, still unresolved.
rawUsingTargets :: PipelineResult -> [UsingTarget]
rawUsingTargets pr = maybe [] (mapMaybe target . fst) (parseResult pr)
  where
    target :: QualifiedStmt Ps -> Maybe UsingTarget
    target qs = case directiveOf qs of
        Using fp _ -> Just $ UsingTarget fp (isOverride qs) (getSpan qs)
        _ -> Nothing
