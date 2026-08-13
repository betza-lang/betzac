module Compilation.DriverSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Betzac.Compilation.Context (CompilationContext (..), FileEntry (feDiagnostics))
import qualified Betzac.Compilation.Driver as Driver
import Betzac.Compilation.Flag (optionsFromFlags)
import Betzac.Diagnostic (SemanticProblem (semKind), causeOf)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog

-- | Every diagnostic recorded on any discovered file.
allDiagnostics :: CompilationContext -> [SemanticProblem]
allDiagnostics ctx = concatMap feDiagnostics (Map.elems (ccFiles ctx))

hasCause :: String -> [SemanticProblem] -> Bool
hasCause c = any ((== c) . causeOf . semKind)

-- | Write a chain @f0 -> f1 -> ... -> f(n-1)@, each file @using@-ing the next, with
-- its own unique export (a single uppercase letter, per the label grammar). Every
-- file is reachable from @f0@.
writeChain :: FilePath -> Int -> IO ()
writeChain dir n = forM_ [0 .. n - 1] $ \i -> do
    let usingLine = if i < n - 1 then "using f" ++ show (i + 1) ++ ";\n" else ""
        content = usingLine ++ "export " ++ [toEnum (fromEnum 'A' + i)] ++ " = fW;\n"
    writeFile (dir </> ("f" ++ show i ++ ".betza")) content

spec :: Spec
spec = describe "Compilation.Driver" $ do
    describe "discover" $ do
        it "reports a system failure when the workspace root doesn't exist" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "export X = fW;\n"
                result <- Driver.discover (dir </> "nope") (dir </> "f0.betza") (optionsFromFlags [])
                case result of
                    Left problem -> causeOf (semKind problem) `shouldBe` "system"
                    Right _ -> fail "expected a system failure for a missing workspace root"

        it "reports a system failure when the target file doesn't exist" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                result <- Driver.discover dir (dir </> "nope.betza") (optionsFromFlags [])
                case result of
                    Left problem -> causeOf (semKind problem) `shouldBe` "system"
                    Right _ -> fail "expected a system failure for a missing target file"

        it "reports using unknown for an unresolvable using target, without aborting discovery" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "using nope;\nexport X = fW;\n"
                result <- Driver.discover dir (dir </> "f0.betza") (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> hasCause "using unknown" (allDiagnostics ctx) `shouldBe` True

        it "only discovers a diamond dependency once" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "using f1;\nusing f2;\nexport A = fW;\n"
                writeFile (dir </> "f1.betza") "using f3;\nexport B = fW;\n"
                writeFile (dir </> "f2.betza") "using f3;\nexport C = fW;\n"
                writeFile (dir </> "f3.betza") "export D = fW;\n"
                result <- Driver.discover dir (dir </> "f0.betza") (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> Map.size (ccFiles ctx) `shouldBe` 4

    describe "discover on generated dependency chains" $ do
        it "discovers every file in an acyclic chain exactly once, with no errors" $
            hedgehog $ do
                n <- forAll $ Gen.int (Range.linear 2 6)
                result <- liftIO $ withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                    writeChain dir n
                    Driver.discover dir (dir </> "f0.betza") (optionsFromFlags [])
                case result of
                    Left problem -> annotate (causeOf (semKind problem)) >> failure
                    Right ctx -> do
                        Map.size (ccFiles ctx) === n
                        assert (null (allDiagnostics ctx))

        it "terminates and reports using circular on a chain closed into a cycle" $
            hedgehog $ do
                n <- forAll $ Gen.int (Range.linear 2 6)
                result <- liftIO $ withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                    writeChain dir n
                    -- Close the chain into a cycle: the last file also uses the first.
                    appendFile (dir </> ("f" ++ show (n - 1) ++ ".betza")) "using f0;\n"
                    Driver.discover dir (dir </> "f0.betza") (optionsFromFlags [])
                case result of
                    Left problem -> annotate (causeOf (semKind problem)) >> failure
                    Right ctx -> do
                        Map.size (ccFiles ctx) === n
                        assert (hasCause "using circular" (allDiagnostics ctx))
