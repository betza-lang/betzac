{-# LANGUAGE OverloadedStrings #-}

module HandlersSpec (spec) where

import Control.Lens
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import System.Directory (canonicalizePath)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Language.LSP.Protocol.Lens hiding (context, length)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Hspec

betzaKind :: LanguageKind
betzaKind = LanguageKind_Custom "betza"

blsSession :: Session () -> IO ()
blsSession = runSession "bls" fullLatestClientCaps "."

spec :: Spec
spec = describe "bls handlers" $ do
    describe "onOpen" $ do
        context "with valid expressions" $ do
            it "produces no diagnostics for a simple atom" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "W;") >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a modified atom" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fW;") >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a chained expression" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fW-[B];") >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a complex expression" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fWbF;") >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for whitespace between tokens" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fW bF;") >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

        context "with invalid expressions" $ do
            it "produces a diagnostic for a character outside the alphabet" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fW@bF;") >> waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

            it "reports the diagnostic at the correct position" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "fW@bF;") >> waitForDiagnostics
                    let pos = diags !! 0 ^. range . start . character
                    liftIO $ pos `shouldBe` 2

            it "produces exactly one diagnostic even for multiple invalid characters" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "@@@@;") >> waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

    describe "onChange" $ do
        context "when correcting an error" $ do
            it "clears diagnostics after invalid input is corrected" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack "fW@bF;")
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fWbF;")
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "updates diagnostics when a new error is introduced" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack "fWbF;")
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fW@bF;")
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

        context "when editing valid expressions" $ do
            it "produces no diagnostics after valid edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack "W;")
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fWbF;")
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "reports updated position after edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack "@;")
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fW@;")
                        ]
                    diags <- waitForDiagnostics
                    let pos = diags !! 0 ^. range . start . character
                    liftIO $ pos `shouldBe` 2

        context "when making multiple edits" $ do
            it "reflects the last edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack "W;")
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fW@;")
                        ]
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "fWbF;")
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ diags `shouldBe` []

    describe "multi-file compilation" $
        it "reports a diagnostic in a using-dependency at the dependency's own location, not the file you opened" $
            withSystemTempDirectory "bls-handlers-spec" $ \rawDir -> do
                dir <- canonicalizePath rawDir
                -- Q is never defined, so dep's own body has exactly one unresolved
                -- reference — but that doesn't block dep.betza from parsing or from
                -- exporting X (a statement's own body being semantically broken doesn't
                -- prevent the statement itself from being exported), so main.betza's
                -- reference to X still resolves and main.betza itself stays clean.
                writeFile (dir </> "dep.betza") "export X = Q;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Y = X;\n"
                let depUri = filePathToUri (dir </> "dep.betza")
                    mainUri = filePathToUri (dir </> "main.betza")
                runSession "bls" fullLatestClientCaps dir $ do
                    _ <- openDoc "main.betza" betzaKind
                    notes <- replicateM 2 publishDiagnosticsNotification
                    let diagsFor u = concat [n ^. params . diagnostics | n <- notes, n ^. params . uri == u]
                    liftIO $ length (diagsFor depUri) `shouldBe` 1
                    liftIO $ diagsFor mainUri `shouldBe` []
