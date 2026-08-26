{-# LANGUAGE OverloadedStrings #-}

module LangServer.Server (
    serverBLS,
) where

import Control.Monad.IO.Class (liftIO)
import LangServer.Cache (BlsCache)
import LangServer.Config (ConfigBLS, defaultConfigBLS, optionsBLS)
import LangServer.Handlers.OnChange
import LangServer.Handlers.OnDefinition (onDefinition)
import LangServer.Handlers.OnOpen
import LangServer.Handlers.OnSemanticTokens (onSemanticTokens)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

handlers :: BlsCache -> ClientCapabilities -> Handlers (LspT ConfigBLS IO)
handlers cache =
    const $
        mconcat
            [ notificationHandler SMethod_TextDocumentDidOpen (onOpen cache)
            , notificationHandler SMethod_TextDocumentDidChange (onChange cache)
            , notificationHandler SMethod_TextDocumentDidClose $ const mempty
            , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ const mempty
            , notificationHandler SMethod_Initialized $ const mempty
            , requestHandler SMethod_TextDocumentSemanticTokensFull (onSemanticTokens cache)
            , requestHandler SMethod_TextDocumentDefinition (onDefinition cache)
            ]

serverBLS :: BlsCache -> ServerDefinition ConfigBLS
serverBLS cache =
    ServerDefinition
        { defaultConfig = defaultConfigBLS
        , configSection = "betzac"
        , parseConfig = \old _value -> Right old
        , onConfigChange = const $ pure ()
        , doInitialize = \env -> const $ pure $ Right env
        , staticHandlers = handlers cache
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = optionsBLS
        }
