{-# LANGUAGE LambdaCase #-}

module Betzac.Parser.Parser (parseTokens, parseTokensRecovering) where

import Betzac.AST.Phases (Ps, PsX (..))
import qualified Betzac.AST.Types as B

import Betzac.Alphabet.Expr (alphanum)
import Betzac.Located (Located (endPos, startPos), tokenVal)
import Betzac.Parser.BetzaTokenStream (BetzaTokenStream (unBetzaTokenStream))
import Betzac.Parser.Core
import Betzac.Span (Span (..))
import qualified Betzac.Token as B

import Control.Applicative (asum)
import Data.Char (isDigit)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Void (Void)
import Text.Megaparsec

-- helpers

{- | 'getSourcePos', over a custom token stream, reports the *start* of whatever
token is next in line (see 'Betzac.Parser.BetzaTokenStream.reachOffset') — correct
for error-position reporting, but not what a span's end marker wants once
inter-token whitespace isn't part of any token's own span (cf. the lexer's
'Betzac.Lexer.Lexer.lexSource'): the position right after the last thing 'p' parsed
is generally *not* the same as the start of whatever comes after it once there was
skipped whitespace/comments in between. Use the last actually-consumed token's own
'endPos' instead.
-}
spanning :: Parser (PsX -> a) -> Parser a
spanning p = do
    st <- getParserState
    let before = unBetzaTokenStream (stateInput st)
        beforeOffset = stateOffset st
    f <- p
    afterOffset <- getOffset
    let consumed = afterOffset - beforeOffset
    case before of
        (t : _) ->
            let s = startPos t
                e
                    | consumed <= 0 = s
                    | otherwise = endPos (before !! (consumed - 1))
             in return $ f $ PsX $ RealSpan s e
        -- At eof there is no token to ask, and nothing was consumed either.
        [] -> do
            s <- getSourcePos
            return $ f $ PsX $ RealSpan s s

withModality ::
    (B.ChainOperator Ps -> Parser (PsX -> a)) -> -- mandatory
    (B.ChainOperator Ps -> Parser (PsX -> a)) -> -- content in braces
    (B.ChainOperator Ps -> Parser (PsX -> a)) -> -- content in brackets
    Parser (PsX -> a)
withModality mandatory iffUnblocked choose = do
    kind <- parseChainKind
    asum
        [ do
            _ <- tok B.TokLBrace
            op <- spanning $ pure $ \x -> B.ChainOperator kind (B.IffUnblocked x) x
            r <- iffUnblocked op
            _ <- tok B.TokRBrace
            return r
        , do
            _ <- tok B.TokLBracket
            op <- spanning $ pure $ \x -> B.ChainOperator kind (B.Choose x) x
            r <- choose op
            _ <- tok B.TokRBracket
            return r
        , do
            op <- spanning $ pure (B.ChainOperator kind (B.Mandatory ()))
            mandatory op
        ]

withOptionalModality ::
    Parser (PsX -> a) ->
    (B.ChainOperator Ps -> Parser (PsX -> a)) ->
    Parser (PsX -> a)
withOptionalModality noOp withOp = optional (withModality withOp withOp withOp) >>= maybe noOp pure

-- parsing

{- | Plain best-effort AST, ignoring any recovered errors (used where callers
already know their input is well-formed, e.g. the parser round-trip test).
-}
parseTokens :: Parser (B.BetzaProgram Ps)
parseTokens = fst <$> parseTokensRecovering

{- | Best-effort AST paired with every error recovered along the way. Errors
are threaded through as plain 'Either' values rather than via megaparsec's own
'registerParseError'/'stateParseErrors' machinery: registering an error from
inside a recovered branch, even though documented as non-failing, was
observed to make the *whole* parse fail outright once wrapped in 'manyTill' —
so accumulation is done by hand here instead.
-}
parseTokensRecovering :: Parser (B.BetzaProgram Ps, [ParseError BetzaTokenStream Void])
parseTokensRecovering = do
    results <- manyTill recoverableStmt (hidden eof)
    let (errs, stmts) = partitionEithers results
    return (stmts, errs)

{- | One statement, best-effort: 'Right' on success; on failure, 'Left' the
error after skipping tokens up to and including the next ';' (or eof,
whichever comes first) — the same statement boundary 'parseQualifiedStmt'
itself requires.
-}
recoverableStmt :: Parser (Either (ParseError BetzaTokenStream Void) (B.QualifiedStmt Ps))
recoverableStmt =
    withRecovery (\e -> Left e <$ recover) (Right <$> parseQualifiedStmt)
  where
    recover = do
        _ <- takeWhileP (Just "recovery: skipped tokens") ((/= B.TokEndStmt) . tokenVal)
        _ <- optional (tok B.TokEndStmt)
        return ()

parseQualifiedStmt :: Parser (B.QualifiedStmt Ps)
parseQualifiedStmt = spanning $ (parseOverride' <|> parseDirective') <* parseEndStmt
  where
    parseOverride' = B.Override <$> parseOverride
    parseDirective' = B.Plain <$> parseDirective
    parseEndStmt = tok B.TokEndStmt <?> "';' to end statement"

parseOverride :: Parser (B.Directive Ps)
parseOverride = tok B.TokOverride *> parseDirective

parseDirective :: Parser (B.Directive Ps)
parseDirective =
    ( parseUsing
        <|> parseExport
        <|> (spanning $ B.Bare <$> parseStmt)
    )
        <?> "statement or directive"

parseUsing :: Parser (B.Directive Ps)
parseUsing = spanning $ dispatch $ \case
    B.TokUsing p -> Just $ B.Using $ toRelativePath p
    _ -> Nothing

toRelativePath :: String -> FilePath
toRelativePath = map (\c -> if c == '.' then '/' else c)

parseExport :: Parser (B.Directive Ps)
parseExport = spanning $ B.Export <$> (tok B.TokExport *> parseStmt)

{- | A label, either alone (a bare reference) or followed by @= expr@ (an
assignment). Parsing the label once up front, rather than wrapping a full
@label = expr@ attempt in 'try', means a real mistake inside 'expr' (e.g. a
dangling modifier with no atom after it) surfaces as its own specific error at
the point it actually occurs, instead of being silently discarded by backtracking
into "well, maybe this was just a bare reference" and reporting a confusing
"expecting ';'" back at the '='.
-}
parseStmt :: Parser (B.BetzaStmt Ps)
parseStmt = spanning $ parseLabel >>= continue
  where
    continue lbl = (B.Assign lbl <$> (tok B.TokAssign *> parseExpr)) <|> pure (B.LabelRef lbl)

parseExpr :: Parser (B.BetzaExpr Ps)
parseExpr = (spanning $ B.BetzaExpr <$> parseChainExpr) <?> "expression"

parseChainExpr :: Parser (B.ChainExpr Ps)
parseChainExpr = spanning $ B.ChainExpr <$> parseUnionExpr <*> optional (try parseChainLeg)

parseChainLeg :: Parser (B.ChainLeg Ps)
parseChainLeg = (spanning $ withModality leg leg leg) <?> "chain continuation"
  where
    leg op = B.ChainLeg op <$> parseChainExpr

parseChainKind :: Parser (B.ChainKind Ps)
parseChainKind = spanning $ parseStep <|> parseSequence
  where
    parseStep = B.Step <$ tok B.TokChainStep
    parseSequence = B.Sequence <$ tok B.TokChainSequence

parseUnionExpr :: Parser (B.UnionExpr Ps)
parseUnionExpr = spanning $ B.UnionExpr <$> NE.fromList <$> some parseModifierExpr

parseModifierExpr :: Parser (B.ModifierExpr Ps)
parseModifierExpr =
    ( spanning $
        B.ModifierExpr
            <$> (isJust <$> optional (tok B.TokBang))
            <*> many parseModifier
            <*> parseExponentExpr
    )
        <?> "atom or modifier"

parseModifier :: Parser (B.Modifier Ps)
parseModifier = spanning $ parseDirectional' <|> parseBehavioural'
  where
    parseDirectional' = B.Directional <$> parseDirectional
    parseBehavioural' = B.Behavioural <$> parseBehaviour

parseDirectional :: Parser (B.DirectionModifier Ps)
parseDirectional = spanning $ parseAmalgamated <|> (B.Single <$> parseDirection)
  where
    parseAmalgamated =
        B.Amalgamated
            <$> (tok B.TokLAngle *> parseDirection)
            <*> (parseDirection <* tok B.TokRAngle)

parseDirection :: Parser (B.Direction Ps)
parseDirection = spanning $ dispatch $ \case
    B.TokDirection 'f' -> Just B.Forward
    B.TokDirection 'b' -> Just B.Backward
    B.TokDirection 'l' -> Just B.Leftward
    B.TokDirection 'r' -> Just B.Rightward
    B.TokDirection 's' -> Just B.Sideway
    B.TokDirection 'v' -> Just B.Vertically
    B.TokDirection 'a' -> Just B.All
    _ -> Nothing

parseBehaviour :: Parser (B.Behaviour Ps)
parseBehaviour = spanning $ do
    kind <- parseBehaviourKind
    modality <- parseBehaviourModality kind <|> (pure $ B.Once ())
    return $ B.Behaviour kind modality

parseBehaviourKind :: Parser (B.BehaviourKind Ps)
parseBehaviourKind = spanning $ dispatch $ \case
    B.TokBehaviour 'c' -> Just B.Capture
    B.TokBehaviour 'g' -> Just B.Leap
    B.TokBehaviour 'i' -> Just B.Initial
    B.TokBehaviour 'j' -> Just B.Jump
    B.TokBehaviour 'm' -> Just B.Move
    B.TokBehaviour 'n' -> Just B.NoJump
    B.TokBehaviour 'p' -> Just B.Hop
    _ -> Nothing

parseBehaviourModality :: B.BehaviourKind Ps -> Parser (B.BehaviourModality Ps)
parseBehaviourModality kind = case kind of
    B.Capture _ -> parseTwice (B.TokBehaviour 'c') <|> parseAny
    B.Leap _ -> parseTwice (B.TokBehaviour 'g') <|> parseAny
    B.Jump _ -> parseTwice (B.TokBehaviour 'j') <|> parseAny
    B.Hop _ -> parseTwice (B.TokBehaviour 'p') -- 'y' does not mean anything for hopping
    _ -> empty
  where
    parseTwice t = spanning $ B.Twice <$ tok t
    parseAny = spanning $ B.Any <$ tok (B.TokBehaviour 'y')

parseExponentExpr :: Parser (B.ExponentExpr Ps)
parseExponentExpr = spanning $ B.ExponentExpr <$> parseAtomExpr <*> optional (try parseExponent)

parseExponent :: Parser (B.Exponent Ps)
parseExponent =
    ( spanning $
        withOptionalModality
            (B.Exponent Nothing <$> many parseModifier <*> parseExponentKind)
            (\op -> B.Exponent (Just op) <$> many parseModifier <*> parseExponentKind)
    )
        <?> "exponent"

parseExponentKind :: Parser (B.ExponentKind Ps)
parseExponentKind =
    (spanning $ parseInfinite <|> parseSlippery <|> parseRepeat)
        <?> "repeat count, '0' for infinite, or '0*' for slippery"
  where
    parseInfinite = B.Infinite <$ tok (B.TokNumber 0)
    parseSlippery = B.Slippery <$ tok B.TokSlippery
    parseRepeat = B.Repeat <$> parseNumber <?> "number"
    parseNumber = dispatch $ \case
        B.TokNumber n -> Just n
        _ -> Nothing

parseAtomExpr :: Parser (B.AtomExpr Ps)
parseAtomExpr =
    (spanning $ (B.Paren <$> parseParen) <|> (B.From <$> parseLabel)) <?> "atom"
  where
    parseParen = tok B.TokLParen *> parseExpr <* tok B.TokRParen

parseLabel :: Parser (B.Label Ps)
parseLabel =
    ( spanning $
        dispatch $ \case
            B.TokAtom c -> Just $ B.Upper c
            B.TokDescriptor d -> (\l x -> l x) <$> parseDescriptor d
            _ -> Nothing
    )
        <?> "piece name"

parseDescriptor :: String -> Maybe (PsX -> B.Label Ps)
parseDescriptor s = case span isDigit s of
    (n, ',' : m) | not (null n), all isDigit m, not (null m) -> Just $ B.Leaper (read n) (read m)
    _ -> if validDescriptor s then Just $ B.Descriptor s else Nothing
  where
    validDescriptor (c : cs) =
        c `elem` alphanum
            && last (c : cs) `elem` alphanum
            && noConsecutiveSpaces (c : cs)
    validDescriptor [] = False

    noConsecutiveSpaces (c : cs)
        | c == ' ' = case cs of
            ' ' : _ -> False
            _ -> noConsecutiveSpaces cs
        | c `elem` alphanum = noConsecutiveSpaces cs
        | otherwise = False
    noConsecutiveSpaces [] = True
