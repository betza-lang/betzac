{-# LANGUAGE OverloadedStrings #-}

module LangServer.Handlers.Core (publishDiagnostics, makeDiagnostic) where

import qualified Betzac.Pipeline as B (PipelineResult (..), fromScratch)

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import LangServer.Config
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (publishDiagnostics)
import Text.Megaparsec

publishDiagnostics :: FilePath -> Uri -> Text -> LspM ConfigBLS ()
publishDiagnostics fp u src = do
    let diags = case B.fromScratch fp src of
            Left _ -> []
            Right result -> lexDiags result ++ parseDiags result
    sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams u Nothing diags

lexDiags :: B.PipelineResult -> [Diagnostic]
lexDiags result = case B.lexResult result of
    Just (Left bundle) -> bundleToDiagnostics bundle
    _ -> []

parseDiags :: B.PipelineResult -> [Diagnostic]
parseDiags result = case B.parseResult result of
    Just (Left bundle) -> bundleToDiagnostics bundle
    _ -> []

bundleToDiagnostics ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
    ParseErrorBundle s e -> [Diagnostic]
bundleToDiagnostics bundle =
    [ makeDiagnostic (sourcePosPairToRange sp) (T.pack $ parseErrorTextPretty err)
    | (err, sp) <-
        NE.toList $
            fst $
                attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
    ]

sourcePosPairToRange :: SourcePos -> Range
sourcePosPairToRange sp =
    let pos = sourcePosToPosition sp
     in Range pos pos

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition sp =
    Position
        (fromIntegral $ unPos (sourceLine sp) - 1)
        (fromIntegral $ unPos (sourceColumn sp) - 1)

makeDiagnostic :: Range -> Text -> Diagnostic
makeDiagnostic range message =
    Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "betzac") message Nothing Nothing Nothing
