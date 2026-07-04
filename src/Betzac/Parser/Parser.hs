{-# LANGUAGE LambdaCase #-}

module Betzac.Parser.Parser (parseTokens) where

import Betzac.AST (Ps, PsX (..))
import qualified Betzac.AST as B
import Betzac.Alphabet.Expr (alphanum)
import Betzac.Parser.Core
import qualified Betzac.Token as B

import Betzac.Located (Span (..))
import Control.Applicative (asum)
import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Text.Megaparsec

-- helpers

spanning :: Parser (PsX -> a) -> Parser a
spanning p = do
    s <- getSourcePos
    f <- p
    e <- getSourcePos
    return $ f $ PsX $ RealSpan s e

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

parseTokens :: Parser (B.BetzaProgram Ps)
parseTokens = many parseQualifiedStmt <* hidden eof

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
    parseUsing
        <|> parseExport
        <|> (spanning $ B.Bare <$> parseStmt)
        <?> "statement or directive"

parseUsing :: Parser (B.Directive Ps)
parseUsing = spanning $ dispatch $ \case
    B.TokUsing p -> Just $ B.Using $ toRelativePath p
    _ -> Nothing

toRelativePath :: String -> FilePath
toRelativePath = map (\c -> if c == '.' then '/' else c)

parseExport :: Parser (B.Directive Ps)
parseExport = spanning $ B.Export <$> (tok B.TokExport *> parseStmt)

parseStmt :: Parser (B.BetzaStmt Ps)
parseStmt = spanning $ try parseAssign <|> parseAnonymous
  where
    parseAnonymous = B.Anonymous <$> parseExpr
    parseAssign = B.Assign <$> parseLabel <* tok B.TokAssign <*> parseExpr

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
