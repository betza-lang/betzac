{-# LANGUAGE DataKinds #-}

module LangServer.Handlers.OnOpen (onOpen) where

import Control.Lens ((^.))
import LangServer.Config (ConfigBLS)
import LangServer.Handlers.Core (publishDiagnostics)
import Language.LSP.Protocol.Lens (
    HasParams (params),
    HasTextDocument (textDocument),
    text,
    uri,
 )
import Language.LSP.Protocol.Message (
    Method (Method_TextDocumentDidOpen),
    TNotificationMessage,
 )
import Language.LSP.Protocol.Types (uriToFilePath)
import Language.LSP.Server (LspM)

onOpen :: TNotificationMessage Method_TextDocumentDidOpen -> LspM ConfigBLS ()
onOpen msg = do
    let doc = msg ^. params . textDocument
        u = doc ^. uri
        f = maybe "" id $ uriToFilePath u
    publishDiagnostics f u (doc ^. text)
