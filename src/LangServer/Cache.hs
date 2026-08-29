{- | What bls keeps between requests. Nothing here is an optimization of a single
compile: it is what lets one keystroke's work serve the requests that follow it,
since an editor asks for diagnostics and semantic tokens on the same unchanged text.
-}
module LangServer.Cache (
    BlsCache,
    CompileKey (..),
    newCache,
    cachingCompiler,
    cachedPipeline,
    previousContext,
    reusableContext,
    rememberContext,
    contextMatches,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Betzac.Compilation.Context (CompilationContext (..), feSourceHash)
import Betzac.Compilation.Driver (SourceCompiler)
import Betzac.Compilation.Interface (hashText)
import Betzac.Pipeline (PipelineResult (..), fromScratch)

{- | What a cached context was compiled for. A context is only reusable for the same
target, workspace root and prelude; anything else is a different compilation job even
when every file's text matches.
-}
data CompileKey = CompileKey
    { ckTarget :: FilePath
    , ckRoot :: FilePath
    , ckPrelude :: Maybe FilePath
    }
    deriving (Eq)

-- | The last successful compile, kept whole so an unchanged request can reuse it.
data LastCompile = LastCompile
    { lcKey :: CompileKey
    , lcContext :: CompilationContext
    }

data CacheState = CacheState
    { csFiles :: Map.Map FilePath PipelineResult
    , csLast :: Maybe LastCompile
    }

newtype BlsCache = BlsCache (MVar CacheState)

newCache :: IO BlsCache
newCache = BlsCache <$> newMVar (CacheState Map.empty Nothing)

{- | A file's cached pipeline, when one was compiled from exactly this text. Keyed by
content rather than by mtime or LSP version: the text bls compiles comes from the VFS
overlay as often as from disk, and content is the only thing both agree on.
-}
cachedPipeline :: BlsCache -> FilePath -> Text -> IO (Maybe PipelineResult)
cachedPipeline (BlsCache var) path src = do
    st <- readMVar var
    return $ case Map.lookup path (csFiles st) of
        Just pr | sourceText pr == src -> Just pr
        _ -> Nothing

{- | A 'SourceCompiler' that reuses a file's last pipeline when its text is unchanged.
The compile itself runs outside the lock, so a slow file never blocks another request;
two concurrent misses both compile and the later write wins, which is harmless because
the entry is keyed by the very text it was compiled from.
-}
cachingCompiler :: BlsCache -> SourceCompiler
cachingCompiler cache@(BlsCache var) path src = do
    hit <- cachedPipeline cache path src
    case hit of
        Just pr -> return $ Right pr
        Nothing -> case fromScratch path src of
            Left err -> return $ Left err
            Right pr -> do
                modifyMVar_ var $ \st -> return st{csFiles = Map.insert path pr (csFiles st)}
                return $ Right pr

{- | The last context compiled for this job, whether or not it is still current. What
it already resolved can be carried into the next compile file by file, which is a
weaker claim than 'reusableContext' makes and holds far more often.
-}
previousContext :: BlsCache -> CompileKey -> IO (Maybe CompilationContext)
previousContext (BlsCache var) key = do
    st <- readMVar var
    return $ case csLast st of
        Just lc | lcKey lc == key -> Just (lcContext lc)
        _ -> Nothing

{- | The last compiled context, when it is still current: same job, and every file it
reached still reads exactly as it did then. Reading goes through the caller's own
reader so an open buffer is compared against, never the stale file behind it.
-}
reusableContext :: BlsCache -> CompileKey -> (FilePath -> IO Text) -> IO (Maybe CompilationContext)
reusableContext cache key readSource = do
    previous <- previousContext cache key
    case previous of
        Nothing -> return Nothing
        Just ctx -> do
            let paths = Map.keys $ ccFiles ctx
            readings <- traverse (try . readSource) paths
            let now = Map.fromList [(p, t) | (p, Right t) <- zip paths (readings :: [Either IOException Text])]
            return $ if contextMatches ctx now then Just ctx else Nothing

rememberContext :: BlsCache -> CompileKey -> CompilationContext -> IO ()
rememberContext (BlsCache var) key ctx =
    modifyMVar_ var $ \st -> return st{csLast = Just (LastCompile key ctx)}

{- | Whether every file a context reached still holds the text it was compiled from.
A file missing from the current reading counts as changed: it can no longer be read,
which is itself a reason to recompile.
-}
contextMatches :: CompilationContext -> Map.Map FilePath Text -> Bool
contextMatches ctx now =
    Map.size now == Map.size (ccFiles ctx)
        && and
            [ (hashText <$> Map.lookup path now) == Just (feSourceHash entry)
            | (path, entry) <- Map.toList (ccFiles ctx)
            ]
