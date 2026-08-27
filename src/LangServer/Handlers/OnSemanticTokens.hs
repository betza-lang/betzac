{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LangServer.Handlers.OnSemanticTokens (onSemanticTokens) where

import Betzac.AST.Generic (Collector (..), universeBy)
import Betzac.AST.Phases (Ps)
import Betzac.AST.Types
import Betzac.Pipeline (PipelineResult (..))
import Betzac.Span (HasSpan (getSpan), Span (..))

import Control.Lens ((^.))
import Data.List (sortOn)
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Typeable (cast)
import Text.Megaparsec.Pos (SourcePos (..), unPos)

import LangServer.Cache (BlsCache)
import LangServer.Config (ConfigBLS)
import LangServer.Handlers.Core (pipelineFor)
import Language.LSP.Protocol.Lens (HasParams (params), HasTextDocument (textDocument), HasUri (uri))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

{- | Semantic highlighting for one document: parse it (single-file only — spans are
purely syntactic, no cross-file resolution needed), walk the AST for tokens of
interest, and encode them against the legend the lsp library already advertises by
default. Live buffer content only (no disk fallback): a request always targets an
open document.
-}
onSemanticTokens ::
    BlsCache ->
    TRequestMessage Method_TextDocumentSemanticTokensFull ->
    (Either (TResponseError Method_TextDocumentSemanticTokensFull) (SemanticTokens |? Null) -> LspM ConfigBLS ()) ->
    LspM ConfigBLS ()
onSemanticTokens cache req responder = do
    let u = req ^. params . textDocument . uri
        fp = maybe "" id (uriToFilePath u)
    mvf <- getVirtualFile (toNormalizedUri u)
    mpr <- pipelineFor cache fp (maybe T.empty virtualFileText mvf)
    let prog = maybe [] (maybe [] fst . parseResult) mpr
        toks = sortOn (\(SemanticTokenAbsolute l c _ _ _) -> (l, c)) (collectTokens prog)
    responder $ Right $ case makeSemanticTokens defaultSemanticTokensLegend toks of
        Right semTokens -> InL semTokens
        Left _ -> InR Null

collectTokens :: BetzaProgram Ps -> [SemanticTokenAbsolute]
collectTokens prog =
    concatMap keywordTokens prog
        ++ universeBy (highlighted (defSpansOf prog)) prog

{- | The one descent every highlighted node is found in. Four separate 'universeOf'
walks would each cross every node of the program to reach their own type.
-}
highlighted :: Set.Set Span -> Collector SemanticTokenAbsolute
highlighted defSpans = Collector $ \node acc ->
    case cast node :: Maybe (Direction Ps) of
        Just d -> directionToken d ++ acc
        Nothing -> case cast node :: Maybe (Behaviour Ps) of
            Just b -> behaviourToken b ++ acc
            Nothing -> case cast node :: Maybe (ChainOperator Ps) of
                Just c -> chainOperatorToken c ++ acc
                Nothing -> case cast node :: Maybe (Label Ps) of
                    Just lbl -> labelToken (definitionModifier defSpans lbl) lbl ++ acc
                    Nothing -> acc

-- | Whether a label occurrence is the one being defined, rather than a reference.
definitionModifier :: Set.Set Span -> Label Ps -> [SemanticTokenModifiers]
definitionModifier defSpans lbl
    | getSpan lbl `Set.member` defSpans = [SemanticTokenModifiers_Definition]
    | otherwise = []

{- | "override"/"export"/"using" don't have their own span-carrying node — each one's
extension field spans the *whole* directive it introduces, but the grammar
guarantees the keyword itself is exactly what starts that span.
-}
keywordTokens :: QualifiedStmt Ps -> [SemanticTokenAbsolute]
keywordTokens o@(Override d _) = keywordAt (getSpan o) "override" ++ directiveKeyword d
keywordTokens (Plain d _) = directiveKeyword d

directiveKeyword :: Directive Ps -> [SemanticTokenAbsolute]
directiveKeyword u@(Using _ _) = keywordAt (getSpan u) "using"
directiveKeyword e@(Export _ _) = keywordAt (getSpan e) "export"
directiveKeyword (Bare _ _) = []

keywordAt :: Span -> String -> [SemanticTokenAbsolute]
keywordAt (RealSpan s _) kw =
    tokenAt
        (unPos (sourceLine s))
        (unPos (sourceColumn s))
        (length kw)
        SemanticTokenTypes_Keyword
        []
keywordAt Generated _ = []

semanticToken :: (HasSpan a) => SemanticTokenTypes -> [SemanticTokenModifiers] -> a -> [SemanticTokenAbsolute]
semanticToken semType semMods tok = wholeToken (getSpan tok) semType semMods

directionToken :: Direction Ps -> [SemanticTokenAbsolute]
directionToken = semanticToken SemanticTokenTypes_Modifier []

behaviourToken :: Behaviour Ps -> [SemanticTokenAbsolute]
behaviourToken = semanticToken SemanticTokenTypes_Modifier []

chainOperatorToken :: ChainOperator Ps -> [SemanticTokenAbsolute]
chainOperatorToken = semanticToken SemanticTokenTypes_Operator []

{- | Where each label a statement defines (the LHS of an assignment) sits, so every
other occurrence of a label can be told apart as a reference.
-}
defSpansOf :: BetzaProgram Ps -> Set.Set Span
defSpansOf prog = Set.fromList [getSpan lbl | qs <- prog, Just lbl <- [assignLabelOf qs]]

labelToken :: [SemanticTokenModifiers] -> Label Ps -> [SemanticTokenAbsolute]
labelToken = semanticToken SemanticTokenTypes_Variable

assignLabelOf :: QualifiedStmt Ps -> Maybe (Label Ps)
assignLabelOf (Override d _) = assignLabelOfDirective d
assignLabelOf (Plain d _) = assignLabelOfDirective d

assignLabelOfDirective :: Directive Ps -> Maybe (Label Ps)
assignLabelOfDirective (Export stmt _) = assignLabelOfStmt stmt
assignLabelOfDirective (Bare stmt _) = assignLabelOfStmt stmt
assignLabelOfDirective (Using _ _) = Nothing

assignLabelOfStmt :: BetzaStmt Ps -> Maybe (Label Ps)
assignLabelOfStmt (Assign lbl _ _) = Just lbl
assignLabelOfStmt (LabelRef _ _) = Nothing

{- | A token spanning a whole (single-line) node. Everything here except
keywords/labels (which have own narrower helpers): directions, behaviours,
chain operators. Skips anything multi-line or zero-width rather than emit a
malformed token.
-}
wholeToken :: Span -> SemanticTokenTypes -> [SemanticTokenModifiers] -> [SemanticTokenAbsolute]
wholeToken (RealSpan s e) semType semMods
    | sourceLine s == sourceLine e && unPos (sourceColumn e) > unPos (sourceColumn s) =
        tokenAt
            (unPos (sourceLine s))
            (unPos (sourceColumn s))
            (unPos (sourceColumn e) - unPos (sourceColumn s))
            semType
            semMods
    | otherwise = []
wholeToken Generated _ _ = []

tokenAt :: Int -> Int -> Int -> SemanticTokenTypes -> [SemanticTokenModifiers] -> [SemanticTokenAbsolute]
tokenAt line col len semType semMods =
    [ SemanticTokenAbsolute
        (fromIntegral (line - 1))
        (fromIntegral (col - 1))
        (fromIntegral len)
        semType
        semMods
    ]
