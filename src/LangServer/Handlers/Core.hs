{-# LANGUAGE OverloadedStrings #-}

module LangServer.Handlers.Core (publishDiagnostics, makeDiagnostic) where

import Betzac.Debug.PrettyPrint (prettyPrint)
import Betzac.Located (Span (..))
import qualified Betzac.Pipeline as B (PipelineResult (..), fromScratch)
import Betzac.Semantic.Core (SemanticProblem (..), Severity (..))

import Text.Megaparsec

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

import LangServer.Config
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (publishDiagnostics)

publishDiagnostics :: FilePath -> Uri -> Text -> LspM ConfigBLS ()
publishDiagnostics fp u src = do
    let diags = case B.fromScratch fp src of
            Left _ -> []
            Right result -> lexDiags result <> parseDiags result <> semanticDiags result
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

semanticDiags :: B.PipelineResult -> [Diagnostic]
semanticDiags result = case B.semanticResult result of
    Nothing -> []
    Just problems -> map semanticProblemToDiagnostic problems

bundleToDiagnostics ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
    ParseErrorBundle s e -> [Diagnostic]
bundleToDiagnostics bundle =
    [ makeDiagnostic (sourcePosPairToRange sp sp) (T.pack $ parseErrorTextPretty err) DiagnosticSeverity_Error
    | (err, sp) <-
        NE.toList $
            fst $
                attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
    ]

semanticProblemToDiagnostic :: SemanticProblem -> Diagnostic
semanticProblemToDiagnostic problem =
    makeDiagnostic
        (toRange . semSpan $ problem)
        (T.pack . prettyPrint . semKind $ problem)
        (getLspSeverity problem)

-- utils

sourcePosPairToRange :: SourcePos -> SourcePos -> Range
sourcePosPairToRange spos epos =
    let s = sourcePosToPosition spos
        e = sourcePosToPosition epos
     in Range s e

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition sp =
    Position
        (fromIntegral $ unPos (sourceLine sp) - 1)
        (fromIntegral $ unPos (sourceColumn sp) - 1)

makeDiagnostic :: Range -> Text -> DiagnosticSeverity -> Diagnostic
makeDiagnostic range message severity =
    Diagnostic range (Just severity) Nothing Nothing (Just "betzac") message Nothing Nothing Nothing

-- Note: Rangeless diagnostics are currently unsupported in the LSP spec
--       See https://github.com/microsoft/language-server-protocol/issues/256
toRange :: Span -> Range
toRange Generated = Range (Position 0 0) (Position 0 0)
toRange (RealSpan s e) = sourcePosPairToRange s e

getLspSeverity :: SemanticProblem -> DiagnosticSeverity
getLspSeverity (SemanticProblem Warning _ _) = DiagnosticSeverity_Warning
getLspSeverity (SemanticProblem Error _ _) = DiagnosticSeverity_Error
