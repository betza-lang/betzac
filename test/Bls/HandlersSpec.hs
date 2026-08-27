{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module HandlersSpec (spec) where

import Control.Exception (bracket_)
import Control.Lens
import Control.Monad (replicateM, (<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Text (pack)
import System.Directory (canonicalizePath)
import System.Environment (setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Language.LSP.Protocol.Lens hiding (context, length, name)
import Language.LSP.Protocol.Types
import Language.LSP.Test
import Test.Hspec

betzaKind :: LanguageKind
betzaKind = LanguageKind_Custom "betza"

blsSession :: Session () -> IO ()
blsSession = runSession "bls" fullLatestClientCaps "."

{- | Defines the atom letters used by the fixtures below as real (self-resolving,
leaper-based) labels, so an expression referencing them doesn't also trip an
unrelated 'UnresolvedLabel' diagnostic on top of whatever the test is after.
Exported (as every tested statement below also is), so none of this ever trips
an 'UnusedLabel' diagnostic either — bls publishes every diagnostic unfiltered,
with no -Wunused gate, so unused-by-construction fixtures would otherwise always
show up.
-}
preamble :: String
preamble = "export W = :1,1:;\nexport B = :1,2:;\nexport F = :2,1:;\n"

-- | A workspace directory bls can resolve against, with a canonical path.
inWorkspace :: (FilePath -> IO a) -> IO a
inWorkspace act = withSystemTempDirectory "bls-handlers-spec" $ act <=< canonicalizePath

-- | Run an action with the standard prelude pointed at a fixture.
withPrelude :: FilePath -> IO a -> IO a
withPrelude path = bracket_ (setEnv "BETZAC_PRELUDE" path) (unsetEnv "BETZAC_PRELUDE")

-- | Where bls says the thing at 'pos' in 'name' is defined.
definitionsAt :: FilePath -> FilePath -> Position -> IO [Location]
definitionsAt dir name pos =
    runSession "bls" fullLatestClientCaps dir $ do
        doc <- openDoc name betzaKind
        locationsOf <$> getDefinitions doc pos

locationsOf :: Definition |? ([DefinitionLink] |? Null) -> [Location]
locationsOf (InL (Definition (InL loc))) = [loc]
locationsOf (InL (Definition (InR locs))) = locs
locationsOf (InR (InL links)) = [Location u r | DefinitionLink (LocationLink _ u r _) <- links]
locationsOf (InR (InR Null)) = []

lineRange :: UInt -> UInt -> UInt -> UInt -> Range
lineRange sl sc el ec = Range (Position sl sc) (Position el ec)

-- | How many semantic tokens an answer carries; the wire format is five ints each.
tokenCount :: SemanticTokens |? Null -> Int
tokenCount (InL (SemanticTokens _ dat)) = length dat `div` 5
tokenCount (InR Null) = 0

spec :: Spec
spec = describe "bls handlers" $ do
    describe "onOpen" $ do
        context "with valid expressions" $ do
            it "produces no diagnostics for a simple atom" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = W;")) >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a modified atom" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW;")) >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a chained expression" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW-[B];")) >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for a complex expression" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fWbF;")) >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "produces no diagnostics for whitespace between tokens" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW bF;")) >> waitForDiagnostics
                    liftIO $ diags `shouldBe` []

        context "with invalid expressions" $ do
            it "produces a diagnostic for a character outside the alphabet" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW@bF;")) >> waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

            it "reports the diagnostic at the correct position" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW@bF;")) >> waitForDiagnostics
                    let pos = diags !! 0 ^. range . start . character
                    liftIO $ pos `shouldBe` 13

            it "produces exactly one diagnostic even for multiple invalid characters" $
                blsSession $ do
                    diags <- createDoc "test.betza" betzaKind (pack "@@@@;") >> waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

    describe "onChange" $ do
        context "when correcting an error" $ do
            it "clears diagnostics after invalid input is corrected" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fW@bF;"))
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fWbF;"))
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "updates diagnostics when a new error is introduced" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = fWbF;"))
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fW@bF;"))
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ length diags `shouldBe` 1

        context "when editing valid expressions" $ do
            it "produces no diagnostics after valid edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = W;"))
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fWbF;"))
                        ]
                    diags <- waitForDiagnostics
                    liftIO $ diags `shouldBe` []

            it "reports updated position after edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = @;"))
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fW@;"))
                        ]
                    diags <- waitForDiagnostics
                    let pos = diags !! 0 ^. range . start . character
                    liftIO $ pos `shouldBe` 13

        context "when making multiple edits" $ do
            it "reflects the last edit" $
                blsSession $ do
                    doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = W;"))
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fW@;"))
                        ]
                    _ <- waitForDiagnostics
                    changeDoc
                        doc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fWbF;"))
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

    describe "reuse between requests" $ do
        it "serves semantic tokens for the text after an edit, not the text before it" $
            blsSession $ do
                doc <- createDoc "test.betza" betzaKind (pack (preamble ++ "export X = W;"))
                _ <- waitForDiagnostics
                before <- getSemanticTokens doc
                changeDoc
                    doc
                    [ TextDocumentContentChangeEvent $
                        InR $
                            TextDocumentContentChangeWholeDocument (pack (preamble ++ "export X = fbslvW;"))
                    ]
                _ <- waitForDiagnostics
                after <- getSemanticTokens doc
                liftIO $ tokenCount after `shouldSatisfy` (> tokenCount before)

        it "sees a dependency's edited buffer from the file that uses it" $
            withSystemTempDirectory "bls-handlers-spec" $ \rawDir -> do
                dir <- canonicalizePath rawDir
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Y = X;\n"
                let mainUri = filePathToUri (dir </> "main.betza")
                runSession "bls" fullLatestClientCaps dir $ do
                    mainDoc <- openDoc "main.betza" betzaKind
                    _ <- replicateM 2 publishDiagnosticsNotification
                    depDoc <- openDoc "dep.betza" betzaKind
                    _ <- publishDiagnosticsNotification
                    -- dep stops exporting X, so main's reference to it no longer resolves.
                    changeDoc
                        depDoc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "export Z = :1,0:;\n")
                        ]
                    _ <- publishDiagnosticsNotification
                    changeDoc
                        mainDoc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "using dep;\nexport Y = X;\n\n")
                        ]
                    notes <- replicateM 2 publishDiagnosticsNotification
                    let diagsFor u = concat [n ^. params . diagnostics | n <- notes, n ^. params . uri == u]
                    liftIO $ length (diagsFor mainUri) `shouldBe` 1

        it "re-resolves against a dependency that changed on disk without ever being opened" $
            withSystemTempDirectory "bls-handlers-spec" $ \rawDir -> do
                dir <- canonicalizePath rawDir
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Y = X;\n"
                let mainUri = filePathToUri (dir </> "main.betza")
                runSession "bls" fullLatestClientCaps dir $ do
                    mainDoc <- openDoc "main.betza" betzaKind
                    _ <- replicateM 2 publishDiagnosticsNotification
                    -- Behind the editor's back: no didOpen, no didChange for dep.
                    liftIO $ writeFile (dir </> "dep.betza") "export Z = :1,0:;\n"
                    changeDoc
                        mainDoc
                        [ TextDocumentContentChangeEvent $
                            InR $
                                TextDocumentContentChangeWholeDocument (pack "using dep;\nexport Y = X;\n\n")
                        ]
                    notes <- replicateM 2 publishDiagnosticsNotification
                    let diagsFor u = concat [n ^. params . diagnostics | n <- notes, n ^. params . uri == u]
                    liftIO $ length (diagsFor mainUri) `shouldBe` 1

    describe "go to definition" $ do
        it "jumps to a definition in the same file" $
            inWorkspace $ \dir -> do
                writeFile (dir </> "main.betza") "export W = :1,0:;\nexport K = W;\n"
                locs <- definitionsAt dir "main.betza" (Position 1 11)
                locs `shouldBe` [Location (filePathToUri (dir </> "main.betza")) (lineRange 0 0 0 17)]

        it "jumps into the file a using directive pulled the definition in from" $
            inWorkspace $ \dir -> do
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Y = X;\n"
                locs <- definitionsAt dir "main.betza" (Position 1 11)
                map (view uri) locs `shouldBe` [filePathToUri (dir </> "dep.betza")]

        it "jumps to the file a using directive names, from the path itself" $
            inWorkspace $ \dir -> do
                writeFile (dir </> "dep.betza") "export X = :1,0:;\n"
                writeFile (dir </> "main.betza") "using dep;\nexport Y = X;\n"
                locs <- definitionsAt dir "main.betza" (Position 0 6)
                locs `shouldBe` [Location (filePathToUri (dir </> "dep.betza")) (lineRange 0 0 0 0)]

        it "jumps into the standard prelude for a label nothing local defines" $
            inWorkspace $ \dir -> do
                let prelude = dir </> "std.betza"
                writeFile prelude "export N = :2,1:;\n"
                writeFile (dir </> "main.betza") "export K = N;\n"
                locs <- withPrelude prelude $ definitionsAt dir "main.betza" (Position 0 11)
                map (view uri) locs `shouldBe` [filePathToUri prelude]

        it "jumps from a definition being outranked to the import that displaces it" $
            inWorkspace $ \dir -> do
                writeFile (dir </> "dep.betza") "export N = :3,1:;\n"
                writeFile (dir </> "main.betza") "using dep;\nN = :2,1:;\nexport K = N;\n"
                locs <- definitionsAt dir "main.betza" (Position 1 0)
                map (view uri) locs `shouldBe` [filePathToUri (dir </> "dep.betza")]

        it "answers with nothing for a position on no label at all" $
            inWorkspace $ \dir -> do
                writeFile (dir </> "main.betza") "export W = :1,0:;\nexport K = W;\n"
                locs <- definitionsAt dir "main.betza" (Position 1 6)
                locs `shouldBe` []
