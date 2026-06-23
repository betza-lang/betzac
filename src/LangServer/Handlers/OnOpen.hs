{-# LANGUAGE DataKinds #-}

module LangServer.Handlers.OnOpen (onOpen) where

import Control.Lens ((^.))
import LangServer.Config (ConfigBLS)
import LangServer.Handlers.Core
import Language.LSP.Protocol.Lens (
    HasParams (params),
    HasTextDocument (textDocument),
    text,
    uri,
 )
import Language.LSP.Protocol.Message
import Language.LSP.Server (LspM)

onOpen :: TNotificationMessage Method_TextDocumentDidOpen -> LspM ConfigBLS ()
onOpen msg = do
    let doc = msg ^. params . textDocument
    publishDiagnostics (doc ^. uri) (doc ^. text)
