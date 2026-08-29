{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LangServer.Handlers.OnDefinition (onDefinition) where

import Betzac.AST.Generic (universeOf)
import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (Label)
import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    ResolvedDef (..),
    UsingTarget (..),
    rdOrigin,
 )
import Betzac.Compilation.Label.Scope (labelText)
import qualified Betzac.Pipeline as B (PipelineResult (parseResult))
import Betzac.Span (HasSpan (getSpan), Span (..))

import Control.Lens ((^.))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Text.Megaparsec.Pos (SourcePos (..), unPos)

import LangServer.Cache (BlsCache)
import LangServer.Config (ConfigBLS)
import LangServer.Handlers.Core (contextFor)
import Language.LSP.Protocol.Lens (HasParams (params), HasPosition (position), HasTextDocument (textDocument), HasUri (uri))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

{- | Go to source for the thing under the cursor: a @using@ directive resolves to the
file it names, and a label -- whether written as a reference or as the left-hand side
of its own assignment -- resolves to whichever definition wins in this file's effective
scope, in whatever file that turns out to be, prelude included.
-}
onDefinition ::
    BlsCache ->
    TRequestMessage Method_TextDocumentDefinition ->
    (Either (TResponseError Method_TextDocumentDefinition) (Definition |? ([DefinitionLink] |? Null)) -> LspM ConfigBLS ()) ->
    LspM ConfigBLS ()
onDefinition cache req responder = do
    let u = req ^. params . textDocument . uri
        pos = req ^. params . position
    case uriToFilePath u of
        Nothing -> responder $ Right nowhere
        Just fp -> do
            mvf <- getVirtualFile (toNormalizedUri u)
            result <- contextFor cache fp (maybe T.empty virtualFileText mvf)
            responder $ Right $ case result of
                Left _ -> nowhere
                Right ctx -> maybe nowhere (InL . Definition . InL) (definitionAt ctx fp pos)

-- | The empty answer: nothing under the cursor leads anywhere.
nowhere :: Definition |? ([DefinitionLink] |? Null)
nowhere = InR (InR Null)

{- | Where the cursor points, if anywhere. @using@ directives are consulted first:
their span covers the path, which holds no label of its own.
-}
definitionAt :: CompilationContext -> FilePath -> Position -> Maybe Location
definitionAt ctx fp pos = do
    entry <- Map.lookup fp (ccFiles ctx)
    usingTarget entry `orElse` labelTarget entry
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

    usingTarget entry =
        wholeFile . usingPath
            <$> lookupSpan (feUsingTargets entry)

    labelTarget entry = do
        lbl <- lookupSpan (labels entry)
        eff <- feEffective entry
        rd <- Map.lookup (labelText lbl) eff
        pure $ Location (filePathToUri $ rdOrigin rd) (toRange $ getSpan (rdDef rd))

    labels entry = universeOf (maybe [] fst (B.parseResult (fePipeline entry))) :: [Label Ps]

    lookupSpan :: (HasSpan a) => [a] -> Maybe a
    lookupSpan = foldr (\x acc -> if covers (getSpan x) pos then Just x else acc) Nothing

-- | The whole file, selecting nothing: where a @using@ directive leads.
wholeFile :: FilePath -> Location
wholeFile path = Location (filePathToUri path) (Range start start)
  where
    start = Position 0 0

{- | Whether a span contains an LSP position. LSP counts lines and characters from 0,
megaparsec from 1; the end column is exclusive, as it is for a token's own extent.
-}
covers :: Span -> Position -> Bool
covers Generated _ = False
covers (RealSpan s e) (Position line char) =
    (startLine, startChar) <= here && here < (endLine, endChar)
  where
    here = (fromIntegral line + 1, fromIntegral char + 1) :: (Int, Int)
    startLine = unPos (sourceLine s)
    startChar = unPos (sourceColumn s)
    endLine = unPos (sourceLine e)
    endChar = unPos (sourceColumn e)

toRange :: Span -> Range
toRange Generated = Range (Position 0 0) (Position 0 0)
toRange (RealSpan s e) = Range (toPosition s) (toPosition e)

toPosition :: SourcePos -> Position
toPosition sp =
    Position
        (fromIntegral $ unPos (sourceLine sp) - 1)
        (fromIntegral $ unPos (sourceColumn sp) - 1)
