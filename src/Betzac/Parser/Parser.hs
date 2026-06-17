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
parseQualifiedStmt = parseOverride <|> (Plain <$> parseDirective)

parseOverride :: Parser QualifiedStmt
parseOverride = Override <$> (tok TokOverride *> parseDirective)

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
parseStmt = (parseLabelHeaded <|> parseAnonymous) <* tok TokEndStmt
  where
    parseAnonymous = Anonymous <$> parseExpr

parseLabelHeaded :: Parser BetzaStmt
parseLabelHeaded = do
    lhs <- parseLabel
    (tok TokAssign *> (parseAlias lhs <|> parseAssign lhs)) <|> parseResolve lhs

parseAlias :: Label -> Parser BetzaStmt
parseAlias lhs = Alias lhs <$> parseLabel

parseAssign :: Label -> Parser BetzaStmt
parseAssign lhs = Assign lhs <$> parseExpr

parseResolve :: Label -> Parser BetzaStmt
parseResolve lhs = return $ Resolve lhs

parseExpr :: Parser BetzaExpr
parseExpr = BetzaExpr <$> parseChainExpr

parseChainExpr :: Parser ChainExpr
parseChainExpr = ChainExpr <$> parseOptionExpr <*> many ((,) <$> parseChainOperator <*> parseOptionExpr)

parseChainOperator :: Parser ChainOperator
parseChainOperator = parseStep <|> parseSequence
  where
    parseStep = Step <$ tok TokChainStep
    parseSequence = Sequence <$ tok TokChainSequence

parseOptionExpr :: Parser OptionExpr
parseOptionExpr = parseChoose <|> parseIffUnblocked <|> parseMandatory

parseChoose :: Parser OptionExpr
parseChoose = Choose <$> (tok TokLBrace *> parseExpr <* tok TokRBrace)

parseIffUnblocked :: Parser OptionExpr
parseIffUnblocked = IffUnblocked <$> (tok TokLBracket *> parseExpr <* tok TokRBracket)

parseMandatory :: Parser OptionExpr
parseMandatory = Mandatory <$> parseUnionExpr

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
parseExponent = parseInfinite <|> parseRepeat <|> parseSlippery
  where
    parseInfinite = Infinite <$ tok (TokNumber 0)
    parseSlippery = Slippery <$ tok TokSlippery
    parseRepeat = Repeat <$> optional parseChainOperator <*> many parseModifier <*> parseNumber
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
