{-# LANGUAGE OverloadedStrings #-}

module LangServer.Handlers.Core (offsetToPosition, spanToRange, pointRange, publishDiagnostics, lexDiags, parseDiags, makeDiagnostic) where

import Betzac.Lexer.Core (LexError (..))
import Betzac.Parser.Core (ParseError (..))
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Data.Text (Text)
import qualified Data.Text as T
import LangServer.Config
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (publishDiagnostics)

offsetToPosition :: [Char] -> Int -> Position
offsetToPosition src offset =
    let before = take offset src
        (line, col) =
            foldl
                (\(l, c) ch -> if ch == '\n' then (l + 1, 0) else (l, c + 1))
                (0, 0)
                before
     in Position line col

spanToRange :: String -> (Int, Int) -> Range
spanToRange src (s, e) = Range (offsetToPosition src s) (offsetToPosition src e)

pointRange :: String -> Int -> Range
pointRange src offset =
    let pos = offsetToPosition src offset
     in Range pos pos

publishDiagnostics :: Uri -> Text -> LspM ConfigBLS ()
publishDiagnostics u src = do
    let srcStr = T.unpack src
        diags = case fromScratch src of
            Left _ -> []
            Right result -> lexDiags srcStr result ++ parseDiags srcStr result
    sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams u Nothing diags

lexDiags :: String -> PipelineResult -> [Diagnostic]
lexDiags src result = case lexResult result of
    Just (Left (LexError pos)) ->
        [makeDiagnostic (pointRange src pos) "Unexpected character"]
    _ -> []

parseDiags :: String -> PipelineResult -> [Diagnostic]
parseDiags src result = case parseResult result of
    Just (Left err) ->
        let range = case parseErrSpan err of
                Just (s, e) -> spanToRange src (s, e)
                Nothing -> pointRange src 0
         in [makeDiagnostic range "Parse error"]
    _ -> []

makeDiagnostic :: Range -> Text -> Diagnostic
makeDiagnostic range message =
    Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "betzac") message Nothing Nothing Nothing