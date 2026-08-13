{-# LANGUAGE OverloadedStrings #-}

module Betzac.Lexer.Lexer (lexSource, runLexer) where

import qualified Betzac.Alphabet.Expr as B
import qualified Betzac.Alphabet.Stmt as B
import Betzac.Located (Located (..))
import qualified Betzac.Token as B

import Betzac.Lexer.Core
import Betzac.Lexer.Space

import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

runLexer :: FilePath -> String -> Either (ParseErrorBundle String Void) [Located B.Token]
runLexer = parse lexSource

-- | 'spanned' wraps only 'lexToken'' (not the trailing 'lexIgnore') so a token's
-- captured span ends exactly at its own last character, not after whatever
-- whitespace/comment happens to follow it.
lexSource :: Lexer [Located B.Token]
lexSource = lexIgnore *> many (spanned lexToken' <* lexIgnore) <* hidden eof
  where
    lexToken' = lexDirective <|> lexToken

lexDirective :: Lexer B.Token
lexDirective = lexExport <|> lexUsing <|> lexOverride <?> "directive keyword"

lexToken :: Lexer B.Token
lexToken =
    lexAtom
        <|> lexDescriptor
        <|> lexDirection
        <|> lexBehaviour
        <|> lexParen
        <|> lexBracket
        <|> lexBrace
        <|> lexAngle
        <|> lexChain
        <|> lexBang
        <|> lexNumber
        <|> lexComma
        <|> lexAssign
        <|> lexEndStmt
        <?> "valid character as part of a token"

lexAtom :: Lexer B.Token
lexAtom = B.TokAtom <$> satisfy (`elem` B.upper)

lexDescriptor :: Lexer B.Token
lexDescriptor = try $ B.TokDescriptor <$> (char ':' *> descriptor <* char ':')
  where
    descriptor = takeWhile1P (Just "descriptor character") (\c -> c `elem` [',', B.space] || c `elem` B.alphanum)

lexDirection :: Lexer B.Token
lexDirection = B.TokDirection <$> satisfy (`elem` B.direction)

lexBehaviour :: Lexer B.Token
lexBehaviour = B.TokBehaviour <$> satisfy (`elem` B.behaviour)

lexParen :: Lexer B.Token
lexParen = lparen <|> rparen
  where
    lparen = B.TokLParen <$ char '('
    rparen = B.TokRParen <$ char ')'

lexBracket :: Lexer B.Token
lexBracket = lbracket <|> rbracket
  where
    lbracket = B.TokLBracket <$ char '['
    rbracket = B.TokRBracket <$ char ']'

lexBrace :: Lexer B.Token
lexBrace = lbrace <|> rbrace
  where
    lbrace = B.TokLBrace <$ char '{'
    rbrace = B.TokRBrace <$ char '}'

lexAngle :: Lexer B.Token
lexAngle = langle <|> rangle
  where
    langle = B.TokLAngle <$ char '<'
    rangle = B.TokRAngle <$ char '>'

lexChain :: Lexer B.Token
lexChain = char '-' *> (B.TokChainSequence <$ char '-' <|> pure B.TokChainStep)

lexBang :: Lexer B.Token
lexBang = B.TokBang <$ char '!'

lexNumber :: Lexer B.Token
lexNumber = try lexZeroStar <|> lexNumeric
  where
    lexZeroStar = B.TokSlippery <$ (char '0' *> char '*')
    lexNumeric = B.TokNumber <$> L.decimal

lexComma :: Lexer B.Token
lexComma = B.TokComma <$ char ','

lexAssign :: Lexer B.Token
lexAssign = B.TokAssign <$ char B.assign

lexEndStmt :: Lexer B.Token
lexEndStmt = B.TokEndStmt <$ char B.stmtEnd

lexKeyword :: String -> B.Token -> Lexer B.Token
lexKeyword kw t = t <$ string kw <* notFollowedBy (oneOf B.alphanum)

lexOverride :: Lexer B.Token
lexOverride = lexKeyword "override" B.TokOverride

lexExport :: Lexer B.Token
lexExport = lexKeyword "export" B.TokExport

lexUsing :: Lexer B.Token
lexUsing = B.TokUsing <$> (string "using" *> lexIgnoreSome *> path)
  where
    path = (<>) <$> part <*> (concat <$> (many $ try $ (:) <$> char '.' <*> part))
    part = takeWhile1P (Just "path character") (`elem` B.alphanum)
