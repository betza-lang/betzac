{-# LANGUAGE OverloadedStrings #-}

module LangServer.Handlers.Core (makeDiagnostic, publishLexDiagnostics) where

import Betzac.Lexer.Core
import Betzac.Lexer.Lexer (lexSource)
import Data.Text
import LangServer.Config
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

makeDiagnostic :: Int -> Text -> Diagnostic
makeDiagnostic pos message =
    let
        position = Position 0 (fromIntegral pos)
        range = Range position position
        severity = Just DiagnosticSeverity_Error
     in
        Diagnostic range severity Nothing Nothing (Just "betzac") message Nothing Nothing Nothing

publishLexDiagnostics :: Uri -> String -> LspM ConfigBLS ()
publishLexDiagnostics u source = do
    let diags = either toDiag (const []) (runLexer lexSource source) where
        toDiag (LexError pos) = [makeDiagnostic pos "Unexpected character"]
    sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams u Nothing diags