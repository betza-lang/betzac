{-# LANGUAGE ScopedTypeVariables #-}

{- | Where compiled interfaces live between runs. The store is a cache, never a source
of truth: an unreadable or unwritable one costs a rebuild and nothing else.
-}
module Betzac.Compilation.Driver.Store (
    InterfaceStore,
    storeAt,
    defaultStore,
    storePath,
    interfacesOf,
    readStored,
    writeStored,
    publishInterfaces,
    usableInterface,
    interfacedEntry,
) where

import Control.Exception (IOException, try)
import Control.Monad (foldM, void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath ((<.>), (</>))

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileBody (Interfaced),
    FileEntry (..),
    FileStatus (FromInterface),
    feIsClean,
    feSourceHash,
 )
import Betzac.Compilation.Driver.Resolve (dependenciesOf)
import Betzac.Compilation.Interface (
    DependencyStamp (..),
    Interface (..),
    hashText,
    ieLabel,
    interfaceHash,
    parseInterface,
    renderHash,
    renderInterface,
 )

newtype InterfaceStore = InterfaceStore FilePath

storeAt :: FilePath -> InterfaceStore
storeAt = InterfaceStore

{- | Under the user's cache directory rather than beside the source: the standard
prelude ships in a read-only install tree, and a workspace is not ours to litter.
-}
defaultStore :: IO InterfaceStore
defaultStore = storeAt <$> getXdgDirectory XdgCache ("betzac" </> "bi")

-- | Named by the digest of the source's canonical path, so any source tree is cacheable.
storePath :: InterfaceStore -> FilePath -> FilePath
storePath (InterfaceStore root) source = root </> renderHash (hashText $ T.pack source) <.> "bi"

{- | Every discovered file's interface. Dependencies are acyclic, so the table may be
tied lazily: a file's stamps read the entries this same map is still producing.
-}
interfacesOf :: CompilationContext -> Map.Map FilePath Interface
interfacesOf ctx = table
  where
    table = Map.mapWithKey build (ccFiles ctx)
    build path entry =
        Interface
            { ifSource = feSourceHash entry
            , ifDeps = [DependencyStamp d (interfaceHash i) | d <- dependenciesOf ctx path entry, Just i <- [Map.lookup d table]]
            , ifExports = maybe [] Map.elems $ feExported entry
            }

readStored :: InterfaceStore -> FilePath -> IO (Maybe Interface)
readStored store source = do
    contents <- try $ TIO.readFile $ storePath store source
    return $ case contents of
        Left (_ :: IOException) -> Nothing
        Right t -> either (const Nothing) Just $ parseInterface t

{- | An unwritable cache is a missed optimisation, never an error. Written aside and
renamed, so a reader never meets half a file.
-}
writeStored :: InterfaceStore -> FilePath -> Interface -> IO ()
writeStored store@(InterfaceStore root) source i = void (try attempt :: IO (Either IOException ()))
  where
    final = storePath store source
    attempt = do
        createDirectoryIfMissing True root
        TIO.writeFile (final <.> "tmp") (renderInterface i)
        renameFile (final <.> "tmp") final

{- | Store the interface of every file that compiled with nothing to report. A file
carrying so much as a warning is left uncached: reusing it would mean deciding whether
to replay diagnostics nobody recomputed.
-}
publishInterfaces :: InterfaceStore -> CompilationContext -> IO ()
publishInterfaces store ctx =
    mapM_ publish $ Map.toList $ interfacesOf ctx
  where
    publish (path, i)
        | maybe False feIsClean $ Map.lookup path (ccFiles ctx) = writeStored store path i
        | otherwise = return ()

{- | The stored interface for a file, when it still describes what is on disk: the text
it was produced from is the text there now, and every dependency is itself usable with
the hash it was stamped with. Reading through the caller's own reader is what keeps an
editor's unsaved buffer from ever matching.
-}
usableInterface :: InterfaceStore -> (FilePath -> IO Text) -> FilePath -> IO (Maybe Interface)
usableInterface store readSource = fmap snd . check Set.empty Map.empty
  where
    check onPath memo path
        -- Only corrupt stamps can cycle; refusing them costs a rebuild.
        | path `Set.member` onPath = return (memo, Nothing)
        | Just remembered <- Map.lookup path memo = return (memo, remembered)
        | otherwise = do
            stored <- readStored store path
            current <- maybe (return False) (freshFor path) stored
            case stored of
                Just i | current -> do
                    (memo', intact) <- foldM (stamp $ Set.insert path onPath) (memo, True) (ifDeps i)
                    settle memo' path $ if intact then Just i else Nothing
                _ -> settle memo path Nothing

    stamp _ (memo, False) _ = return (memo, False)
    stamp onPath (memo, True) d = do
        (memo', dep) <- check onPath memo (dsPath d)
        return (memo', maybe False ((== dsHash d) . interfaceHash) dep)

    settle memo path verdict = return (Map.insert path verdict memo, verdict)

    freshFor path i = do
        text <- try $ readSource path
        return $ case text of
            Left (_ :: IOException) -> False
            Right t -> hashText t == ifSource i

-- | A file nobody had to compile, known only by what its dependents may see.
interfacedEntry :: Interface -> FileEntry
interfacedEntry i =
    FileEntry
        { feBody = Interfaced i
        , feUsingTargets = []
        , feExported = Just $ Map.fromList [(ieLabel e, e) | e <- ifExports i]
        , feEffective = Nothing
        , feDirectiveProblems = []
        , feScopeProblems = []
        , feStatus = FromInterface
        }
