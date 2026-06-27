{-# LANGUAGE LambdaCase #-}

module Betzac.Parser.Parser (parseTokens) where

import qualified Betzac.AST as B
import Betzac.Alphabet.Expr (alphanum)
import Betzac.Parser.Core
import qualified Betzac.Token as B

import Data.Char (isDigit)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Text.Megaparsec

parseTokens :: Parser B.BetzaProgram
parseTokens = many parseQualifiedStmt <* hidden eof

parseQualifiedStmt :: Parser B.QualifiedStmt
parseQualifiedStmt = (parseOverride' <|> parseDirective') <* parseEndStmt
  where
    parseOverride' = B.Override <$> parseOverride
    parseDirective' = B.Plain <$> parseDirective
    parseEndStmt = tok B.TokEndStmt <?> "';' to end statement"

parseOverride :: Parser B.Directive
parseOverride = tok B.TokOverride *> parseDirective

parseDirective :: Parser B.Directive
parseDirective =
    parseUsing <|> parseExport <|> B.Bare
        <$> parseStmt
            <?> "statement or directive"

parseUsing :: Parser B.Directive
parseUsing = dispatch $ \case
    B.TokUsing p -> Just $ B.Using $ toRelativePath p
    _ -> Nothing

toRelativePath :: String -> FilePath
toRelativePath = map (\c -> if c == '.' then '/' else c)

parseExport :: Parser B.Directive
parseExport = B.Export <$> (tok B.TokExport *> parseStmt)

parseStmt :: Parser B.BetzaStmt
parseStmt = try parseAssign <|> parseAnonymous
  where
    parseAnonymous = B.Anonymous <$> parseExpr
    parseAssign = B.Assign <$> parseLabel <* tok B.TokAssign <*> parseExpr

parseExpr :: Parser B.BetzaExpr
parseExpr = B.BetzaExpr <$> parseChainExpr <?> "expression"

parseChainExpr :: Parser B.ChainExpr
parseChainExpr = B.ChainExpr <$> parseUnionExpr <*> optional (try parseChainLeg)

parseChainLeg :: Parser B.ChainLeg
parseChainLeg = (parseChainKind >>= \kind -> parseChoose kind <|> parseIffUnblocked kind <|> parseMandatory kind) <?> "chain continuation"
  where
    parseMandatory k = B.ChainLeg (B.ChainOperator k B.Mandatory) <$> parseChainExpr
    parseIffUnblocked k = B.ChainLeg (B.ChainOperator k B.IffUnblocked) <$> (tok B.TokLBrace *> parseChainExpr <* tok B.TokRBrace)
    parseChoose k = B.ChainLeg (B.ChainOperator k B.Choose) <$> (tok B.TokLBracket *> parseChainExpr <* tok B.TokRBracket)

parseChainKind :: Parser B.ChainKind
parseChainKind = parseStep <|> parseSequence
  where
    parseStep = B.Step <$ tok B.TokChainStep
    parseSequence = B.Sequence <$ tok B.TokChainSequence

parseUnionExpr :: Parser B.UnionExpr
parseUnionExpr = B.UnionExpr <$> NE.fromList <$> some parseModifierExpr

parseModifierExpr :: Parser B.ModifierExpr
parseModifierExpr =
    ( B.ModifierExpr
        <$> (isJust <$> optional (tok B.TokBang))
        <*> many parseModifier
        <*> parseExponentExpr
    )
        <?> "atom or modifier"

parseModifier :: Parser B.Modifier
parseModifier = parseDirectional' <|> parseBehavioural'
  where
    parseDirectional' = B.Directional <$> parseDirectional
    parseBehavioural' = B.Behavioural <$> parseBehaviour

parseDirectional :: Parser B.DirectionModifier
parseDirectional = parseAmalgamated <|> (B.Single <$> parseDirection)
  where
    parseAmalgamated =
        B.Amalgamated
            <$> (tok B.TokLAngle *> parseDirection)
            <*> (parseDirection <* tok B.TokRAngle)

parseDirection :: Parser B.Direction
parseDirection = dispatch $ \case
    B.TokDirection 'f' -> Just B.Forward
    B.TokDirection 'b' -> Just B.Backward
    B.TokDirection 'l' -> Just B.Leftward
    B.TokDirection 'r' -> Just B.Rightward
    B.TokDirection 's' -> Just B.Sideway
    B.TokDirection 'v' -> Just B.Vertically
    B.TokDirection 'a' -> Just B.All
    _ -> Nothing

parseBehaviour :: Parser B.Behaviour
parseBehaviour = do
    kind <- takeKind
    modality <- (takeModality kind) <|> return B.Once
    return $ B.Behaviour kind modality
  where
    takeKind = dispatch $ \case
        B.TokBehaviour 'c' -> Just B.Capture
        B.TokBehaviour 'g' -> Just B.Leap
        B.TokBehaviour 'i' -> Just B.Initial
        B.TokBehaviour 'j' -> Just B.Jump
        B.TokBehaviour 'm' -> Just B.Move
        B.TokBehaviour 'n' -> Just B.NoJump
        B.TokBehaviour 'p' -> Just B.Hop
        -- B.TokBehaviour 'y' not allowed by itself
        _ -> Nothing
    takeModality k = case k of
        B.Capture -> parseTwice (B.TokBehaviour 'c') <|> parseAny
        B.Leap -> parseTwice (B.TokBehaviour 'g') <|> parseAny
        B.Jump -> parseTwice (B.TokBehaviour 'j') <|> parseAny
        B.Hop -> parseTwice (B.TokBehaviour 'p') -- 'y' does not mean anything for hopping
        _ -> empty
    parseTwice t = B.Twice <$ tok t
    parseAny = B.Any <$ tok (B.TokBehaviour 'y')

parseExponentExpr :: Parser B.ExponentExpr
parseExponentExpr = B.ExponentExpr <$> parseAtomExpr <*> optional (try parseExponent)

parseExponent :: Parser B.Exponent
parseExponent =
    ( do
        mop <- optional parseChainKind
        case mop of
            Nothing -> B.Exponent Nothing <$> many parseModifier <*> parseExponentKind
            Just kind -> parseChoose kind <|> parseIffUnblocked kind <|> parseMandatory kind
    )
        <?> "exponent"
  where
    parseMandatory k = B.Exponent (Just $ B.ChainOperator k B.Mandatory) <$> many parseModifier <*> parseExponentKind
    parseIffUnblocked k = B.Exponent (Just $ B.ChainOperator k B.IffUnblocked) <$> (tok B.TokLBrace *> many parseModifier) <*> parseExponentKind <* tok B.TokRBrace
    parseChoose k = B.Exponent (Just $ B.ChainOperator k B.Choose) <$> (tok B.TokLBracket *> many parseModifier) <*> parseExponentKind <* tok B.TokRBracket

parseExponentKind :: Parser B.ExponentKind
parseExponentKind =
    parseInfinite
        <|> parseSlippery
        <|> parseRepeat
        <?> "repeat count, '0' for infinite, or '0*' for slippery"
  where
    parseInfinite = B.Infinite <$ tok (B.TokNumber 0)
    parseSlippery = B.Slippery <$ tok B.TokSlippery
    parseRepeat = B.Repeat <$> parseNumber <?> "number"
    parseNumber = dispatch $ \case
        B.TokNumber n -> Just n
        _ -> Nothing

parseAtomExpr :: Parser B.AtomExpr
parseAtomExpr =
    (B.Paren <$> parseParen)
        <|> (B.From <$> parseLabel)
        <?> "atom"
  where
    parseParen = tok B.TokLParen *> parseExpr <* tok B.TokRParen

parseLabel :: Parser B.Label
parseLabel =
    dispatch
        ( \case
            B.TokAtom c -> Just $ B.Upper c
            B.TokDescriptor d -> parseDescriptor d
            _ -> Nothing
        )
        <?> "piece name"

parseDescriptor :: String -> Maybe B.Label
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
