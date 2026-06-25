{-# LANGUAGE OverloadedStrings #-}

module Betzac.Lexer.Lexer (lexSource, runLexer) where

import qualified Betzac.Alphabet.Expr as B
import Betzac.Located (Located (..))
import qualified Betzac.Token as B

import Betzac.Lexer.Core
import Betzac.Lexer.Space

import Betzac.Alphabet.Expr (alphanum)
import qualified Betzac.Alphabet.Stmt as B
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)

runLexer :: FilePath -> String -> Either (ParseErrorBundle String Void) [Located B.Token]
runLexer = parse lexSource

lexSource :: Lexer [Located B.Token]
lexSource = lexIgnore *> many (spanned $ lexToken' <* lexIgnore) <* hidden eof
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
lexAtom = B.TokAtom <$> oneOf B.upper

lexDescriptor :: Lexer B.Token
lexDescriptor = try $ B.TokDescriptor <$> (char ':' *> descriptor <* char ':')
  where
    descriptor = some (oneOf $ ',' : B.space : B.alphanum)

lexDirection :: Lexer B.Token
lexDirection = B.TokDirection <$> oneOf B.direction

lexBehaviour :: Lexer B.Token
lexBehaviour = B.TokBehaviour <$> oneOf B.behaviour

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
lexNumber = try lexZeroStar <|> try lexNonZero <|> lexZero
  where
    lexZeroStar = B.TokSlippery <$ (char '0' *> char '*')
    lexNonZero = B.TokNumber . read <$> lexPosIntStr
    lexZero = B.TokNumber 0 <$ char '0'

lexPosIntStr :: Lexer String
lexPosIntStr = (:) <$> oneOf B.nonzeroDigit <*> many (oneOf B.digit)

lexComma :: Lexer B.Token
lexComma = B.TokComma <$ char ','

lexAssign :: Lexer B.Token
lexAssign = B.TokAssign <$ char B.assign

lexEndStmt :: Lexer B.Token
lexEndStmt = B.TokEndStmt <$ char B.stmtEnd

lexKeyword :: String -> B.Token -> Lexer B.Token
lexKeyword kw t = t <$ string kw <* notFollowedBy (oneOf alphanum)

lexOverride :: Lexer B.Token
lexOverride = lexKeyword "override" B.TokOverride

lexExport :: Lexer B.Token
lexExport = lexKeyword "export" B.TokExport

lexUsing :: Lexer B.Token
lexUsing = B.TokUsing <$> (string "using" *> lexIgnoreSome *> path)
  where
    path = (<>) <$> part <*> (concat <$> (many $ try $ (:) <$> char '.' <*> part))
    part = some $ oneOf B.alphanum
