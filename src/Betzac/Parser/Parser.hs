{-# LANGUAGE LambdaCase #-}

module Betzac.Parser.Parser (parseTokens) where

import Betzac.AST
import Betzac.Parser.Core
import Betzac.Token

import Data.Char (isDigit)
import Data.Maybe (isJust)

parseTokens :: Parser BetzaProgram
parseTokens = do
    ast <- many parseQualifiedStmt
    rest <- peek
    case rest of
        Nothing -> return ast
        Just _ -> empty

parseQualifiedStmt :: Parser QualifiedStmt
parseQualifiedStmt = ((Override <$> parseOverride) <|> (Plain <$> parseDirective)) <* tok TokEndStmt

parseOverride :: Parser Directive
parseOverride = tok TokOverride *> parseDirective

parseDirective :: Parser Directive
parseDirective = parseUsing <|> parseExport <|> Bare <$> parseStmt

parseUsing :: Parser Directive
parseUsing = dispatch $ \case
    TokUsing p -> Just $ Using $ toRelativePath p
    _ -> Nothing

toRelativePath :: String -> FilePath
toRelativePath = map (\c -> if c == '.' then '/' else c)

parseExport :: Parser Directive
parseExport = Export <$> (tok TokExport *> parseStmt)

parseStmt :: Parser BetzaStmt
parseStmt = parseAssign <|> parseAnonymous
  where
    parseAnonymous = Anonymous <$> parseExpr
    parseAssign = Assign <$> parseLabel <* tok TokAssign <*> parseExpr

parseExpr :: Parser BetzaExpr
parseExpr = BetzaExpr <$> parseChainExpr

parseChainExpr :: Parser ChainExpr
parseChainExpr = ChainExpr <$> parseUnionExpr <*> optional parseChainLeg

parseChainLeg :: Parser ChainLeg
parseChainLeg = do
    kind <- parseChainKind
    parseChoose kind <|> parseIffUnblocked kind <|> parseMandatory kind
  where
    parseMandatory k = ChainLeg (ChainOperator k Mandatory) <$> parseChainExpr
    parseIffUnblocked k = ChainLeg (ChainOperator k IffUnblocked) <$> (tok TokLBrace *> parseChainExpr <* tok TokRBrace)
    parseChoose k = ChainLeg (ChainOperator k Choose) <$> (tok TokLBracket *> parseChainExpr <* tok TokRBracket)

parseChainKind :: Parser ChainKind
parseChainKind = parseStep <|> parseSequence
  where
    parseStep = Step <$ tok TokChainStep
    parseSequence = Sequence <$ tok TokChainSequence

parseUnionExpr :: Parser UnionExpr
parseUnionExpr = UnionExpr <$> someNE parseModifierExpr

parseModifierExpr :: Parser ModifierExpr
parseModifierExpr =
    ModifierExpr
        <$> (isJust <$> optional (tok TokBang))
        <*> many parseModifier
        <*> parseExponentExpr

parseModifier :: Parser Modifier
parseModifier = (Directional <$> parseDirectional) <|> (Behavioural <$> parseBehaviour)

parseDirectional :: Parser DirectionModifier
parseDirectional = parseAmalgamated <|> (Single <$> parseDirection)
  where
    parseAmalgamated =
        Amalgamated
            <$> (tok TokLAngle *> parseDirection)
            <*> (parseDirection <* tok TokRAngle)

parseDirection :: Parser Direction
parseDirection = dispatch $ \case
    TokDirection 'f' -> Just Forward
    TokDirection 'b' -> Just Backward
    TokDirection 'l' -> Just Leftward
    TokDirection 'r' -> Just Rightward
    TokDirection 's' -> Just Sideway
    TokDirection 'v' -> Just Vertically
    TokDirection 'a' -> Just All
    _ -> Nothing

parseBehaviour :: Parser Behaviour
parseBehaviour = dispatch $ \case
    TokBehaviour 'c' -> Just Capture
    TokBehaviour 'g' -> Just Leap
    TokBehaviour 'i' -> Just Initial
    TokBehaviour 'j' -> Just Jump
    TokBehaviour 'm' -> Just Move
    TokBehaviour 'n' -> Just NoJump
    TokBehaviour 'p' -> Just Hop
    TokBehaviour 'y' -> Just Any
    _ -> Nothing

parseExponentExpr :: Parser ExponentExpr
parseExponentExpr = ExponentExpr <$> parseAtomExpr <*> optional parseExponent

parseExponent :: Parser Exponent
parseExponent = do
    mop <- optional parseChainKind
    case mop of
        Nothing -> Exponent Nothing <$> many parseModifier <*> parseExponentKind
        Just kind -> parseChoose kind <|> parseIffUnblocked kind <|> parseMandatory kind
  where
    parseMandatory k = Exponent (Just $ ChainOperator k Mandatory) <$> many parseModifier <*> parseExponentKind
    parseIffUnblocked k = Exponent (Just $ ChainOperator k IffUnblocked) <$> (tok TokLBrace *> many parseModifier) <*> parseExponentKind <* tok TokRBrace
    parseChoose k = Exponent (Just $ ChainOperator k Choose) <$> (tok TokLBracket *> many parseModifier) <*> parseExponentKind <* tok TokRBracket

parseExponentKind :: Parser ExponentKind
parseExponentKind = parseInfinite <|> parseSlippery <|> parseRepeat
  where
    parseInfinite = Infinite <$ tok (TokNumber 0)
    parseSlippery = Slippery <$ tok TokSlippery
    parseRepeat = Repeat <$> parseNumber
    parseNumber = dispatch $ \case
        TokNumber n -> Just n
        _ -> Nothing

parseAtomExpr :: Parser AtomExpr
parseAtomExpr = (Paren <$> parseParen) <|> (From <$> parseLabel)
  where
    parseParen = tok TokLParen *> parseExpr <* tok TokRParen

parseLabel :: Parser Label
parseLabel = dispatch $ \case
    TokAtom c -> Just $ Upper c
    TokDescriptor d -> Just $ parseDescriptor d
    _ -> Nothing

parseDescriptor :: String -> Label
parseDescriptor s = case span isDigit s of
    (n, ',' : m) | not (null n), all isDigit m, not (null m) -> Leaper (read n) (read m)
    _ -> Descriptor s
