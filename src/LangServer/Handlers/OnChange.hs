{-# LANGUAGE DataKinds #-}

module LangServer.Handlers.OnChange (onChange) where

import Control.Lens ((^.))
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import LangServer.Config (ConfigBLS)
import LangServer.Handlers.Core (publishDiagnostics)
import Language.LSP.Protocol.Lens hiding (changes, publishDiagnostics)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (publishDiagnostics)

getChangeText :: TextDocumentContentChangeEvent -> Text
getChangeText (TextDocumentContentChangeEvent (InL partial)) = partial ^. text
getChangeText (TextDocumentContentChangeEvent (InR whole)) = whole ^. text

onChange :: TNotificationMessage Method_TextDocumentDidChange -> LspM ConfigBLS ()
onChange msg = do
    let docId = msg ^. params . textDocument
        changes = msg ^. params . contentChanges
        content = maybe T.empty getChangeText (listToMaybe (reverse changes))
    publishDiagnostics (docId ^. uri) content