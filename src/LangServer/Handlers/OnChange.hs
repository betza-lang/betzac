{-# LANGUAGE DataKinds #-}

module LangServer.Handlers.OnChange (onChange) where

import qualified LangServer.Config as B (ConfigBLS)
import qualified LangServer.Handlers.Core as B (publishDiagnostics)

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens ((^.))
import Language.LSP.Protocol.Lens hiding (changes, id)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

getChangeText :: TextDocumentContentChangeEvent -> Text
getChangeText (TextDocumentContentChangeEvent (InL partial)) = partial ^. text
getChangeText (TextDocumentContentChangeEvent (InR whole)) = whole ^. text

onChange :: TNotificationMessage Method_TextDocumentDidChange -> LspM B.ConfigBLS ()
onChange msg = do
    let docId = msg ^. params . textDocument
        u = docId ^. uri
        f = maybe "" id $ uriToFilePath u
        changes = msg ^. params . contentChanges
        content = maybe T.empty getChangeText (listToMaybe (reverse changes))
    B.publishDiagnostics f u content
