module Compilation.StoreSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Betzac.Compilation.Context (CompilationContext (..))
import qualified Betzac.Compilation.Driver as Driver
import Betzac.Compilation.Driver.Store (
    InterfaceStore,
    interfacesOf,
    publishInterfaces,
    readStored,
    storeAt,
    storePath,
 )
import Betzac.Compilation.Flag (optionsFromFlags)
import Betzac.Compilation.Interface (DependencyStamp (dsHash), Interface (..), ieLabel, ieOrigin, interfaceHash)

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- | Compile a workspace of @(name, source)@ files against one of them, and publish it.
inCompiled :: FilePath -> [(FilePath, String)] -> (FilePath -> InterfaceStore -> CompilationContext -> IO a) -> IO a
inCompiled name files act = withSystemTempDirectory "betzac-store-spec" $ \dir -> do
    mapM_ (\(f, src) -> writeFile (dir </> f) src) files
    let store = storeAt (dir </> "cache")
        target = dir </> name
    discovered <- Driver.discover TIO.readFile dir target Nothing (optionsFromFlags [])
    case discovered of
        Left problem -> fail ("discovery failed: " ++ show problem)
        Right ctx0 -> do
            let ctx = Driver.resolveScopes ctx0
            publishInterfaces store ctx
            act dir store ctx

spec :: Spec
spec = describe "Compilation.Store" $ do
    describe "publishInterfaces" $ do
        it "stores an interface a later run can read back" $
            inCompiled "main.betza" [("main.betza", "export W = :1,0:;\n")] $ \dir store _ -> do
                stored <- readStored store (dir </> "main.betza")
                map ieLabel . ifExports <$> stored `shouldBe` Just ["W"]

        it "credits a re-exported label to the file that defines it"
            $ inCompiled
                "main.betza"
                [ ("main.betza", "using mid;\nexport X;\n")
                , ("mid.betza", "using base;\nexport X;\n")
                , ("base.betza", "export X = :1,0:;\n")
                ]
            $ \dir store _ -> do
                stored <- readStored store (dir </> "main.betza")
                map ieOrigin . ifExports <$> stored `shouldBe` Just [dir </> "base.betza"]

        it "stamps a dependency with the same hash the dependency itself publishes"
            $ inCompiled
                "main.betza"
                [ ("main.betza", "using dep;\nexport Y = X;\n")
                , ("dep.betza", "export X = :1,0:;\n")
                ]
            $ \dir store ctx -> do
                stored <- readStored store (dir </> "main.betza")
                let table = interfacesOf ctx
                case (stored, Map.lookup (dir </> "dep.betza") table) of
                    (Just mainIface, Just depIface) ->
                        map dsHash (ifDeps mainIface) `shouldBe` [interfaceHash depIface]
                    _ -> expectationFailure "expected both interfaces"

        it "leaves a file carrying a warning uncached, so nobody has to replay it" $
            inCompiled "main.betza" [("main.betza", "W = :1,0:;\n")] $ \dir store _ -> do
                -- An unexported, unreferenced label: an unused-label warning.
                stored <- readStored store (dir </> "main.betza")
                stored `shouldSatisfy` isNothing

        it "caches a clean file even when a sibling in the same run is not clean"
            $ inCompiled
                "main.betza"
                [ ("main.betza", "using dep;\nZ = X;\n")
                , ("dep.betza", "export X = :1,0:;\n")
                ]
            $ \dir store _ -> do
                mainIface <- readStored store (dir </> "main.betza")
                depIface <- readStored store (dir </> "dep.betza")
                mainIface `shouldSatisfy` isNothing
                depIface `shouldSatisfy` isJust

    describe "readStored" $ do
        it "misses rather than fails on a store that does not exist" $ do
            stored <- readStored (storeAt "/nonexistent/betzac-store-spec") "/main.betza"
            stored `shouldSatisfy` isNothing

        it "misses rather than fails on an unreadable artifact" $
            withSystemTempDirectory "betzac-store-spec" $ \dir -> do
                let store = storeAt (dir </> "cache")
                createDirectoryIfMissing True (dir </> "cache")
                writeFile (storePath store "/main.betza") "not an interface at all\n"
                stored <- readStored store "/main.betza"
                stored `shouldSatisfy` isNothing

        it "keys artifacts by source path, not by name" $ do
            let store = storeAt "/cache"
            storePath store "/a/main.betza" `shouldSatisfy` (/= storePath store "/b/main.betza")
