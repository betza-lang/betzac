module Compilation.StoreSpec (spec) where

import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (feStatus),
    FileStatus (FromInterface),
    feDiagnostics,
    feIsInterfaced,
 )
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
import Betzac.Diagnostic (SemanticProblem (semKind), causeOf)

import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- | A workspace of @(name, source)@ files, with a store of its own beside them.
inWorkspace :: [(FilePath, String)] -> (FilePath -> InterfaceStore -> IO a) -> IO a
inWorkspace files act = withSystemTempDirectory "betzac-store-spec" $ \dir -> do
    mapM_ (\(f, src) -> writeFile (dir </> f) src) files
    act dir (storeAt (dir </> "cache"))

-- | One compile, reading the store only when given it.
compileIn :: FilePath -> Maybe InterfaceStore -> FilePath -> IO CompilationContext
compileIn dir store name = do
    discovered <- Driver.discover TIO.readFile store dir (dir </> name) Nothing (optionsFromFlags [])
    case discovered of
        Left problem -> fail ("discovery failed: " ++ show problem)
        Right ctx -> return (Driver.resolveScopes ctx)

-- | A first compile, which populates the store.
cold :: FilePath -> InterfaceStore -> FilePath -> IO CompilationContext
cold dir store name = do
    ctx <- compileIn dir Nothing name
    publishInterfaces store ctx
    return ctx

-- | A later compile, free to serve dependencies from what the store holds.
warm :: FilePath -> InterfaceStore -> FilePath -> IO CompilationContext
warm dir store = compileIn dir (Just store)

servedFromInterface :: String -> CompilationContext -> Bool
servedFromInterface name ctx =
    or [feIsInterfaced e | (p, e) <- Map.toList (ccFiles ctx), name `isSuffixOf` p]

causesOn :: String -> CompilationContext -> [String]
causesOn name ctx =
    [causeOf (semKind d) | (p, e) <- Map.toList (ccFiles ctx), name `isSuffixOf` p, d <- feDiagnostics e]

chain :: [(FilePath, String)]
chain = [("main.betza", "using dep;\nexport Y = X;\n"), ("dep.betza", "export X = :1,0:;\n")]

spec :: Spec
spec = describe "Compilation.Store" $ do
    describe "publishInterfaces" $ do
        it "stores an interface a later run can read back" $
            inWorkspace [("main.betza", "export W = :1,0:;\n")] $ \dir store -> do
                _ <- cold dir store "main.betza"
                stored <- readStored store (dir </> "main.betza")
                map ieLabel . ifExports <$> stored `shouldBe` Just ["W"]

        it "credits a re-exported label to the file that defines it"
            $ inWorkspace
                [ ("main.betza", "using mid;\nexport X;\n")
                , ("mid.betza", "using base;\nexport X;\n")
                , ("base.betza", "export X = :1,0:;\n")
                ]
            $ \dir store -> do
                _ <- cold dir store "main.betza"
                stored <- readStored store (dir </> "main.betza")
                map ieOrigin . ifExports <$> stored `shouldBe` Just [dir </> "base.betza"]

        it "stamps a dependency with the same hash the dependency itself publishes" $
            inWorkspace chain $ \dir store -> do
                ctx <- cold dir store "main.betza"
                stored <- readStored store (dir </> "main.betza")
                case (stored, Map.lookup (dir </> "dep.betza") (interfacesOf ctx)) of
                    (Just mainIface, Just depIface) ->
                        map dsHash (ifDeps mainIface) `shouldBe` [interfaceHash depIface]
                    _ -> expectationFailure "expected both interfaces"

        it "leaves a file carrying a warning uncached, so nobody has to replay it" $
            -- An unexported, unreferenced label: an unused-label warning.
            inWorkspace [("main.betza", "W = :1,0:;\n")] $ \dir store -> do
                _ <- cold dir store "main.betza"
                stored <- readStored store (dir </> "main.betza")
                stored `shouldSatisfy` isNothing

        it "caches a clean file even when a sibling in the same run is not clean" $
            inWorkspace [("main.betza", "using dep;\nZ = X;\n"), ("dep.betza", "export X = :1,0:;\n")] $ \dir store -> do
                _ <- cold dir store "main.betza"
                mainIface <- readStored store (dir </> "main.betza")
                depIface <- readStored store (dir </> "dep.betza")
                mainIface `shouldSatisfy` isNothing
                depIface `shouldSatisfy` isJust

    describe "a warm compile" $ do
        it "serves an unchanged dependency from its interface instead of compiling it" $
            inWorkspace chain $ \dir store -> do
                _ <- cold dir store "main.betza"
                ctx <- warm dir store "main.betza"
                servedFromInterface "dep.betza" ctx `shouldBe` True

        it "compiles the target itself however warm the store is" $
            inWorkspace chain $ \dir store -> do
                _ <- cold dir store "main.betza"
                ctx <- warm dir store "main.betza"
                servedFromInterface "main.betza" ctx `shouldBe` False

        it "reports exactly what a cold compile reported" $
            inWorkspace [("main.betza", "using dep;\nZ = X;\n"), ("dep.betza", "export X = :1,0:;\n")] $ \dir store -> do
                coldCtx <- cold dir store "main.betza"
                warmCtx <- warm dir store "main.betza"
                causesOn "main.betza" warmCtx `shouldBe` causesOn "main.betza" coldCtx

        it "resolves the target's scope against a dependency it never parsed" $
            inWorkspace chain $ \dir store -> do
                _ <- cold dir store "main.betza"
                ctx <- warm dir store "main.betza"
                fmap feStatus (Map.lookup (dir </> "dep.betza") (ccFiles ctx)) `shouldBe` Just FromInterface
                -- X resolves, so no unresolved-label error is attributed to main.
                causesOn "main.betza" ctx `shouldBe` []

        it "recompiles a dependency whose source has changed since" $
            inWorkspace chain $ \dir store -> do
                _ <- cold dir store "main.betza"
                writeFile (dir </> "dep.betza") "export X = :2,1:;\n"
                ctx <- warm dir store "main.betza"
                servedFromInterface "dep.betza" ctx `shouldBe` False

        it "recompiles a dependency whose stored artifact is corrupt" $
            inWorkspace chain $ \dir store -> do
                _ <- cold dir store "main.betza"
                writeFile (storePath store (dir </> "dep.betza")) "betzac-bi 1\ngarbage\n"
                ctx <- warm dir store "main.betza"
                servedFromInterface "dep.betza" ctx `shouldBe` False

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
