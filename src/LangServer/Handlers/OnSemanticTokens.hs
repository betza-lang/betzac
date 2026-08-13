{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module LangServer.Handlers.OnSemanticTokens (onSemanticTokens) where

import Betzac.AST.Generic (universeOf)
import Betzac.AST.Phases (Ps)
import Betzac.AST.Types
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Betzac.Span (HasSpan (getSpan), Span (..))

import Control.Lens ((^.))
import Data.List (sortOn)
import qualified Data.Text as T
import Text.Megaparsec.Pos (SourcePos (..), unPos)

import LangServer.Config (ConfigBLS)
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
    TRequestMessage Method_TextDocumentSemanticTokensFull ->
    (Either (TResponseError Method_TextDocumentSemanticTokensFull) (SemanticTokens |? Null) -> LspM ConfigBLS ()) ->
    LspM ConfigBLS ()
onSemanticTokens req responder = do
    let u = req ^. params . textDocument . uri
        fp = maybe "" id (uriToFilePath u)
    mvf <- getVirtualFile (toNormalizedUri u)
    let src = maybe T.empty virtualFileText mvf
        prog = case fromScratch fp src of
            Right pr -> maybe [] fst (parseResult pr)
            Left _ -> []
        toks = sortOn (\(SemanticTokenAbsolute l c _ _ _) -> (l, c)) (collectTokens prog)
    responder $ Right $ case makeSemanticTokens defaultSemanticTokensLegend toks of
        Right semTokens -> InL semTokens
        Left _ -> InR Null

collectTokens :: BetzaProgram Ps -> [SemanticTokenAbsolute]
collectTokens prog =
    concatMap keywordTokens prog
        ++ concatMap directionToken (universeOf prog :: [Direction Ps])
        ++ concatMap behaviourToken (universeOf prog :: [Behaviour Ps])
        ++ concatMap chainOperatorToken (universeOf prog :: [ChainOperator Ps])
        ++ labelTokens prog

-- | "override"/"export"/"using" don't have their own span-carrying node — each one's
-- extension field spans the *whole* directive it introduces, but the grammar
-- guarantees the keyword itself is exactly what starts that span.
keywordTokens :: QualifiedStmt Ps -> [SemanticTokenAbsolute]
keywordTokens (Override d ext) = keywordAt (getSpan ext) "override" ++ directiveKeyword d
keywordTokens (Plain d _) = directiveKeyword d

directiveKeyword :: Directive Ps -> [SemanticTokenAbsolute]
directiveKeyword (Using _ ext) = keywordAt (getSpan ext) "using"
directiveKeyword (Export _ ext) = keywordAt (getSpan ext) "export"
directiveKeyword (Bare _ _) = []

keywordAt :: Span -> String -> [SemanticTokenAbsolute]
keywordAt (RealSpan s _) kw =
    [tokenAt (unPos (sourceLine s)) (unPos (sourceColumn s)) (length kw) SemanticTokenTypes_Keyword []]
keywordAt Generated _ = []

directionToken :: Direction Ps -> [SemanticTokenAbsolute]
directionToken d = wholeToken (directionSpan d) SemanticTokenTypes_Operator []

directionSpan :: Direction Ps -> Span
directionSpan (Forward x) = getSpan x
directionSpan (Backward x) = getSpan x
directionSpan (Leftward x) = getSpan x
directionSpan (Rightward x) = getSpan x
directionSpan (Sideway x) = getSpan x
directionSpan (Vertically x) = getSpan x
directionSpan (All x) = getSpan x

behaviourToken :: Behaviour Ps -> [SemanticTokenAbsolute]
behaviourToken (Behaviour _ _ x) = wholeToken (getSpan x) SemanticTokenTypes_Operator []

chainOperatorToken :: ChainOperator Ps -> [SemanticTokenAbsolute]
chainOperatorToken (ChainOperator _ _ x) = wholeToken (getSpan x) SemanticTokenTypes_Operator []

{- | Every label, tagged 'SemanticTokenModifiers_Definition' if it's the label being
defined (the LHS of an assignment) and left untagged otherwise (a reference) —
distinguished by span, since a definition's own label and a reference to it are
never at the same position.
-}
labelTokens :: BetzaProgram Ps -> [SemanticTokenAbsolute]
labelTokens prog = defTokens ++ refTokens
  where
    defs = [lbl | qs <- prog, Just lbl <- [assignLabelOf qs]]
    defSpans = map labelSpan defs
    defTokens = concatMap (labelToken [SemanticTokenModifiers_Definition]) defs
    refs = [lbl | lbl <- universeOf prog, labelSpan lbl `notElem` defSpans]
    refTokens = concatMap (labelToken []) refs

assignLabelOf :: QualifiedStmt Ps -> Maybe (Label Ps)
assignLabelOf (Override d _) = assignLabelOfDirective d
assignLabelOf (Plain d _) = assignLabelOfDirective d

assignLabelOfDirective :: Directive Ps -> Maybe (Label Ps)
assignLabelOfDirective (Export stmt _) = assignLabelOfStmt stmt
assignLabelOfDirective (Bare stmt _) = assignLabelOfStmt stmt
assignLabelOfDirective (Using _ _) = Nothing

assignLabelOfStmt :: BetzaStmt Ps -> Maybe (Label Ps)
assignLabelOfStmt (Assign lbl _ _) = Just lbl
assignLabelOfStmt (Anonymous _ _) = Nothing

labelSpan :: Label Ps -> Span
labelSpan (Upper _ x) = getSpan x
labelSpan (Descriptor _ x) = getSpan x
labelSpan (Leaper _ _ x) = getSpan x

labelToken :: [SemanticTokenModifiers] -> Label Ps -> [SemanticTokenAbsolute]
labelToken mods lbl = wholeToken (labelSpan lbl) SemanticTokenTypes_Variable mods

-- | A token spanning a whole (single-line) node — everything here except
-- keywords/labels (which have their own narrower helpers): directions, behaviours,
-- chain operators. Skips anything multi-line or zero-width rather than emit a
-- malformed token.
wholeToken :: Span -> SemanticTokenTypes -> [SemanticTokenModifiers] -> [SemanticTokenAbsolute]
wholeToken (RealSpan s e) ty mods
    | sourceLine s == sourceLine e && unPos (sourceColumn e) > unPos (sourceColumn s) =
        [tokenAt (unPos (sourceLine s)) (unPos (sourceColumn s)) (unPos (sourceColumn e) - unPos (sourceColumn s)) ty mods]
    | otherwise = []
wholeToken Generated _ _ = []

tokenAt :: Int -> Int -> Int -> SemanticTokenTypes -> [SemanticTokenModifiers] -> SemanticTokenAbsolute
tokenAt line col len ty mods =
    SemanticTokenAbsolute (fromIntegral (line - 1)) (fromIntegral (col - 1)) (fromIntegral len) ty mods
