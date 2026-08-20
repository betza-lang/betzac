module Compilation.DriverSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text.IO as TIO
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (feDiagnostics, feUsingTargets),
    UsingTarget (usingIsOverride),
 )
import qualified Betzac.Compilation.Driver as Driver
import Betzac.Compilation.Flag (optionsFromFlags)
import Betzac.Diagnostic (SemanticProblem (semKind, semSpan), SemanticProblemKind (DuplicateLabel, UnusedUsing), causeOf)
import Betzac.Span (Span (..))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog
import Text.Megaparsec.Pos (SourcePos (sourceColumn, sourceLine), mkPos)

-- | Every diagnostic recorded on any discovered file.
allDiagnostics :: CompilationContext -> [SemanticProblem]
allDiagnostics ctx = concatMap feDiagnostics (Map.elems (ccFiles ctx))

hasCause :: String -> [SemanticProblem] -> Bool
hasCause c = any ((== c) . causeOf . semKind)

{- | Diagnostics recorded specifically on the discovered file whose path ends with
'suffix' (a plain filename, e.g. "main.betza").
-}
diagnosticsOn :: String -> CompilationContext -> [SemanticProblem]
diagnosticsOn suffix ctx =
    concat [feDiagnostics e | (p, e) <- Map.toList (ccFiles ctx), suffix `isSuffixOf` p]

onLine :: Int -> SemanticProblem -> Bool
onLine n p = case semSpan p of
    RealSpan s _ -> sourceLine s == mkPos n
    Generated -> False

{- | Write a chain @f0 -> f1 -> ... -> f(n-1)@, each file @using@-ing the next, with
its own unique export (a single uppercase letter, per the label grammar). Every
file is reachable from @f0@.
-}
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
                result <- Driver.discover TIO.readFile (dir </> "nope") (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left problem -> causeOf (semKind problem) `shouldBe` "system"
                    Right _ -> fail "expected a system failure for a missing workspace root"

        it "reports a system failure when the target file doesn't exist" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                result <- Driver.discover TIO.readFile dir (dir </> "nope.betza") Nothing (optionsFromFlags [])
                case result of
                    Left problem -> causeOf (semKind problem) `shouldBe` "system"
                    Right _ -> fail "expected a system failure for a missing target file"

        it "reports using unknown for an unresolvable using target, without aborting discovery" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "using nope;\nexport X = fW;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> hasCause "using unknown" (allDiagnostics ctx) `shouldBe` True

        it "only discovers a diamond dependency once" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "using f1;\nusing f2;\nexport A = fW;\n"
                writeFile (dir </> "f1.betza") "using f3;\nexport B = fW;\n"
                writeFile (dir </> "f2.betza") "using f3;\nexport C = fW;\n"
                writeFile (dir </> "f3.betza") "export D = fW;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> Map.size (ccFiles ctx) `shouldBe` 4

    describe "using directives" $ do
        let withDep body =
                withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                    writeFile (dir </> "dep.betza") "export N = :2,1:;\nexport M = :1,1:;\n"
                    body dir
            resolve dir main = do
                writeFile (dir </> "main.betza") main
                result <- Driver.discover TIO.readFile dir (dir </> "main.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> pure (Driver.resolveScopes ctx)
            targetsOf ctx =
                concat [feUsingTargets e | (p, e) <- Map.toList (ccFiles ctx), "main.betza" `isSuffixOf` p]

        it "keeps one using per target, reporting the duplicate as a directive rather than a label pile" $
            withDep $ \dir -> do
                ctx <- resolve dir "using dep;\nusing dep;\nexport X = N;\n"
                length (targetsOf ctx) `shouldBe` 1
                map (causeOf . semKind) (allDiagnostics ctx) `shouldBe` ["duplicate directive"]
                all (onLine 2) (diagnosticsOn "main.betza" ctx) `shouldBe` True

        it "keeps the overriding one when a target is named both ways" $
            withDep $ \dir -> do
                ctx <- resolve dir "using dep;\noverride using dep;\nexport X = N;\n"
                map usingIsOverride (targetsOf ctx) `shouldBe` [True]
                hasCause "duplicate directive" (allDiagnostics ctx) `shouldBe` True

        it "warns on a using whose labels are never referenced" $
            withDep $ \dir -> do
                ctx <- resolve dir "using dep;\nexport X = :1,0:;\n"
                map (causeOf . semKind) (allDiagnostics ctx) `shouldBe` ["unused using"]
                all (onLine 1) (diagnosticsOn "main.betza" ctx) `shouldBe` True

        it "warns on a using whose only referenced label is itself dead" $
            withDep $ \dir -> do
                ctx <- resolve dir "using dep;\nY = N;\n"
                hasCause "unused using" (allDiagnostics ctx) `shouldBe` True

        it "stays quiet about a using that a live definition depends on" $
            withDep $ \dir -> do
                ctx <- resolve dir "using dep;\nexport X = N;\n"
                length (allDiagnostics ctx) `shouldBe` 0

    describe "resolveScopes" $ do
        it "spans a duplicate export's diagnostics across the whole statement, including the export keyword" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "export X = fW;\nexport X = fB;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                            startsAtColumn1 p = case semSpan p of
                                RealSpan s _ -> sourceColumn s == mkPos 1
                                Generated -> False
                            onSecondLine = filter (\p -> causeOf (semKind p) `elem` ["duplicate directive", "duplicate label"]) probs
                        hasCause "duplicate directive" probs `shouldBe` True
                        hasCause "duplicate label" probs `shouldBe` True
                        length onSecondLine `shouldBe` 2
                        all startsAtColumn1 onSecondLine `shouldBe` True

        it "reports an unresolved bare reference even when a statement body is also unresolved" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                -- Q and Z are independent failures; neither may mask the other.
                writeFile (dir </> "f0.betza") "export A = Q;\nZ;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                        map (causeOf . semKind) probs
                            `shouldBe` ["unresolved label", "unresolved label"]

        it "stays quiet about a dead bare reference while the file still has an unresolved one" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "export A = Q;\nW = :1,1:;\nW;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                        map (causeOf . semKind) probs `shouldBe` ["unresolved label"]

        it "reports a circular label once, on itself, however many definitions reference it" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                -- Only B is circular. Every :l: doubles the paths down to it, so
                -- reporting per path costs 2^n, and per consumer still costs n.
                writeFile (dir </> "f0.betza") $
                    "B = :1,0:;\noverride export B = B;\n"
                        ++ "export :l0: = B B;\n"
                        ++ "export :l1: = :l0: :l0:;\n"
                        ++ "export :l2: = :l1: :l1:;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                            circular = filter ((== "circular label") . causeOf . semKind) probs
                        length circular `shouldBe` 1

        it "still reports each definition of a mutual cycle, since both are at fault" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "f0.betza") "export A = B;\nexport B = A;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                        map (causeOf . semKind) probs `shouldBe` ["circular label", "circular label"]
        it "warns an override using that promoted nothing, on the directive in the importing file" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "main.betza") "override using dep;\nexport X;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "main.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx0 -> do
                        let ctx = Driver.resolveScopes ctx0
                            mainDiags = diagnosticsOn "main.betza" ctx
                        length (diagnosticsOn "dep.betza" ctx) `shouldBe` 0
                        map (causeOf . semKind) mainDiags `shouldBe` ["unnecessary override"]
                        all (onLine 1) mainDiags `shouldBe` True

        it "stays quiet about an override using that outranks an earlier plain import" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "other.betza") "export X = :1,1:;\n"
                writeFile (dir </> "main.betza") "using other;\noverride using dep;\nexport X;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "main.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx -> do
                        let probs = allDiagnostics (Driver.resolveScopes ctx)
                        hasCause "unnecessary override" probs `shouldBe` False

        it "warns a local plain definition shadowed by an import on its own statement, not on the import" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "lib.betza") "export W = :1,1:;\n"
                writeFile (dir </> "main.betza") "using lib;\nW = :2,2:;\nexport X = W;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "main.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx0 -> do
                        let ctx = Driver.resolveScopes ctx0
                            libDiags = diagnosticsOn "lib.betza" ctx
                            mainDiags = diagnosticsOn "main.betza" ctx
                        length libDiags `shouldBe` 0
                        map (causeOf . semKind) mainDiags `shouldBe` [causeOf DuplicateLabel]
                        all (onLine 2) mainDiags `shouldBe` True -- "W = :2,2:;" is main.betza's own line 2
        it "warns the losing `using` directive, in the importing file, when two plain imports conflict" $
            withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                writeFile (dir </> "libA.betza") "export W = :1,1:;\n"
                writeFile (dir </> "libB.betza") "export W = :2,2:;\n"
                writeFile (dir </> "main.betza") "using libA;\nusing libB;\nexport X = W;\n"
                result <- Driver.discover TIO.readFile dir (dir </> "main.betza") Nothing (optionsFromFlags [])
                case result of
                    Left _ -> fail "did not expect a system failure"
                    Right ctx0 -> do
                        let ctx = Driver.resolveScopes ctx0
                            libADiags = diagnosticsOn "libA.betza" ctx
                            libBDiags = diagnosticsOn "libB.betza" ctx
                            mainDiags = diagnosticsOn "main.betza" ctx
                        length libADiags `shouldBe` 0
                        length libBDiags `shouldBe` 0
                        -- libB lost its only label, so it also carries nothing into main.
                        map (causeOf . semKind) mainDiags
                            `shouldBe` [causeOf DuplicateLabel, causeOf UnusedUsing]
                        -- libA is used first, so it wins; libB's `using` (main.betza line 2) loses.
                        all (onLine 2) mainDiags `shouldBe` True

    describe "discover on generated dependency chains" $ do
        it "discovers every file in an acyclic chain exactly once, with no errors" $
            hedgehog $ do
                n <- forAll $ Gen.int (Range.linear 2 6)
                result <- liftIO $ withSystemTempDirectory "betzac-driver-spec" $ \dir -> do
                    writeChain dir n
                    Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
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
                    Driver.discover TIO.readFile dir (dir </> "f0.betza") Nothing (optionsFromFlags [])
                case result of
                    Left problem -> annotate (causeOf (semKind problem)) >> failure
                    Right ctx -> do
                        Map.size (ccFiles ctx) === n
                        assert (hasCause "using circular" (allDiagnostics ctx))
