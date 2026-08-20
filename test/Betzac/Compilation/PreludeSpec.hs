module Compilation.PreludeSpec (spec) where

import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (feDiagnostics, feEffective),
    ResolvedDef (rdFrom),
 )
import qualified Betzac.Compilation.Driver as Driver
import Betzac.Compilation.Flag (CompilerFlag (GenerateWarnings), CompilerOptions, Wspecifier (..), optionsFromFlags)
import Betzac.Diagnostic (SemanticProblem (semKind), SemanticProblemKind (SystemFailure), causeOf)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Every warning kind is on, so a silence in these tests is a real silence.
allWarnings :: CompilerOptions
allWarnings = optionsFromFlags [GenerateWarnings Wunused, GenerateWarnings Wdirective, GenerateWarnings Wlang]

-- | A minimal stand-in for the shipped prelude, written into the given directory.
writePrelude :: FilePath -> IO FilePath
writePrelude dir = do
    let path = dir </> "std.betza"
    writeFile path "export W = :1,0:;\nexport N = :2,1:;\n"
    return path

-- | Compile 'target' against a prelude, returning the fully resolved context.
compileWith :: FilePath -> FilePath -> Maybe FilePath -> IO (Either SemanticProblem CompilationContext)
compileWith dir target prelude = do
    result <- Driver.discover TIO.readFile dir target prelude allWarnings
    return $ Driver.sealPrelude . Driver.resolveScopes =<< result

diagnosticsOn :: String -> CompilationContext -> [SemanticProblem]
diagnosticsOn suffix ctx =
    concat [feDiagnostics e | (p, e) <- Map.toList (ccFiles ctx), suffix `isSuffixOf` p]

-- | Where the target's effective scope says a label came from.
sourceOf :: String -> CompilationContext -> Maybe FilePath
sourceOf name ctx = do
    entry <- Map.lookup (ccTarget ctx) (ccFiles ctx)
    eff <- feEffective entry
    rdFrom <$> Map.lookup name eff

spec :: Spec
spec = describe "Compilation.Prelude" $ do
    describe "resolvePrelude" $ do
        it "uses an explicitly given path" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                path <- writePrelude dir
                resolved <- Driver.resolvePrelude (Just path)
                resolved `shouldSatisfy` either (const False) ("std.betza" `isSuffixOf`)

        it "falls back to BETZAC_PRELUDE when no path is given" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                path <- writePrelude dir
                setEnv "BETZAC_PRELUDE" path
                resolved <- Driver.resolvePrelude Nothing
                unsetEnv "BETZAC_PRELUDE"
                resolved `shouldSatisfy` either (const False) ("std.betza" `isSuffixOf`)

        it "prefers an explicit path over BETZAC_PRELUDE" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                explicit <- writePrelude dir
                writeFile (dir </> "other.betza") "export W = :1,0:;\n"
                setEnv "BETZAC_PRELUDE" (dir </> "other.betza")
                resolved <- Driver.resolvePrelude (Just explicit)
                unsetEnv "BETZAC_PRELUDE"
                either (const "") id resolved `shouldBe` explicit

        it "reports a system failure for a named prelude that does not exist" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                resolved <- Driver.resolvePrelude (Just (dir </> "nope.betza"))
                either (causeOf . semKind) (const "resolved") resolved
                    `shouldBe` causeOf (SystemFailure "")

    describe "prelude scope" $ do
        it "puts prelude labels in scope with no using directive" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                prelude <- writePrelude dir
                writeFile (dir </> "main.betza") "export K = N;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> fail ("unexpected system failure: " ++ show (semKind problem))
                    Right ctx -> do
                        sourceOf "N" ctx `shouldBe` Just prelude
                        map (causeOf . semKind) (diagnosticsOn "main.betza" ctx) `shouldBe` []

        it "lets a local definition shadow the prelude silently, even with every warning on" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                prelude <- writePrelude dir
                writeFile (dir </> "main.betza") "N = :3,1:;\nexport K = N;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> fail ("unexpected system failure: " ++ show (semKind problem))
                    Right ctx -> do
                        sourceOf "N" ctx `shouldBe` Just (dir </> "main.betza")
                        map (causeOf . semKind) (diagnosticsOn "main.betza" ctx) `shouldBe` []

        it "lets a using dependency shadow the prelude too" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                prelude <- writePrelude dir
                writeFile (dir </> "dep.betza") "export N = :3,1:;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport K = N;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> fail ("unexpected system failure: " ++ show (semKind problem))
                    Right ctx -> do
                        sourceOf "N" ctx `shouldBe` Just (dir </> "dep.betza")
                        map (causeOf . semKind) (diagnosticsOn "main.betza" ctx) `shouldBe` []

        it "reaches every file in the dependency tree, not just the target" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                prelude <- writePrelude dir
                writeFile (dir </> "dep.betza") "export Q = N;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Q;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> fail ("unexpected system failure: " ++ show (semKind problem))
                    Right ctx -> map (causeOf . semKind) (diagnosticsOn "dep.betza" ctx) `shouldBe` []

        it "keeps the prelude's own warnings out of the results" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                let prelude = dir </> "std.betza"
                writeFile prelude "export W = :1,0:;\nD = :2,0:;\n"
                writeFile (dir </> "main.betza") "export K = W;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> fail ("unexpected system failure: " ++ show (semKind problem))
                    Right ctx -> map (causeOf . semKind) (diagnosticsOn "std.betza" ctx) `shouldBe` []

        it "turns an error inside the prelude into a single system failure" $
            withSystemTempDirectory "betzac-prelude-spec" $ \dir -> do
                let prelude = dir </> "std.betza"
                writeFile prelude "export W = %%%;\n"
                writeFile (dir </> "main.betza") "export K = W;\n"
                result <- compileWith dir (dir </> "main.betza") (Just prelude)
                case result of
                    Left problem -> causeOf (semKind problem) `shouldBe` causeOf (SystemFailure "")
                    Right _ -> fail "expected the broken prelude to fail the compilation"
