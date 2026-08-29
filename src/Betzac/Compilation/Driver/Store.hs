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
) where

import Control.Exception (IOException, try)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, getXdgDirectory, renameFile)
import System.FilePath ((<.>), (</>))

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    feIsClean,
 )
import Betzac.Compilation.Driver.Resolve (dependenciesOf)
import Betzac.Compilation.Interface (
    DependencyStamp (..),
    Interface (..),
    hashText,
    interfaceHash,
    parseInterface,
    renderHash,
    renderInterface,
 )
import Betzac.Pipeline (PipelineResult (..))

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
            { ifSource = hashText $ sourceText $ fePipeline entry
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
