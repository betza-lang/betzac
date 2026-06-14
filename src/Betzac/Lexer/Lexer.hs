module Betzac.Lexer.Lexer (
    lexToken,
    runLexer,
    lexSource,
)
where

import Betzac.Alphabet.Expr (alphanum, behaviour, digit, direction, nonzeroDigit, space, upper)
import Betzac.Alphabet.Stmt (assign, stmtEnd)
import Betzac.Lexer.Core
import Betzac.Lexer.Scan (lexIgnore, lexIgnoreSome)
import Betzac.Token

lexSource :: Lexer [Token]
lexSource = do
    lexIgnore
    tokens <- many (lexToken' <* lexIgnore)
    rest <- peek
    case rest of
        Nothing -> pure tokens
        Just _ -> empty
  where
    lexToken' = lexToken <|> lexDirective

lexDirective :: Lexer Token
lexDirective = lexExport <|> lexUsing <|> lexOverride

lexToken :: Lexer Token
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

lexAtom :: Lexer Token
lexAtom = TokAtom <$> oneOf upper

lexDescriptor :: Lexer Token
lexDescriptor = TokDescriptor <$> (char ':' *> descriptor <* char ':')
  where
    descriptor = some (oneOf $ ',' : space : alphanum)

lexDirection :: Lexer Token
lexDirection = TokDirection <$> oneOf direction

lexBehaviour :: Lexer Token
lexBehaviour = TokBehaviour <$> oneOf behaviour

lexParen :: Lexer Token
lexParen = lparen <|> rparen
  where
    lparen = TokLParen <$ char '('
    rparen = TokRParen <$ char ')'

lexBracket :: Lexer Token
lexBracket = lbracket <|> rbracket
  where
    lbracket = TokLBracket <$ char '['
    rbracket = TokRBracket <$ char ']'

lexBrace :: Lexer Token
lexBrace = lbrace <|> rbrace
  where
    lbrace = TokLBrace <$ char '{'
    rbrace = TokRBrace <$ char '}'

lexAngle :: Lexer Token
lexAngle = langle <|> rangle
  where
    langle = TokLAngle <$ char '<'
    rangle = TokRAngle <$ char '>'

lexChain :: Lexer Token
lexChain = char '-' *> (TokChainSequence <$ char '-' <|> pure TokChainStep)

lexBang :: Lexer Token
lexBang = TokBang <$ char '!'

lexPosIntStr :: Lexer String
lexPosIntStr = (:) <$> oneOf nonzeroDigit <*> many (oneOf digit)

lexNumber :: Lexer Token
lexNumber = lexZeroStar <|> lexNonZero <|> lexZero
  where
    lexZeroStar = TokSlippery <$ (char '0' *> char '*')
    lexZero = TokNumber 0 <$ char '0'
    lexNonZero = TokNumber . read <$> lexPosIntStr

lexComma :: Lexer Token
lexComma = TokComma <$ char ','

lexAssign :: Lexer Token
lexAssign = TokAssign <$ char assign

lexEndStmt :: Lexer Token
lexEndStmt = TokEndStmt <$ char stmtEnd

lexKeyword :: String -> Token -> Lexer Token
lexKeyword kw tok = tok <$ match kw <* lexIgnoreSome

lexOverride :: Lexer Token
lexOverride = lexKeyword "override" TokOverride

lexExport :: Lexer Token
lexExport = lexKeyword "export" TokExport

lexUsing :: Lexer Token
lexUsing = TokUsing <$> (match "using" *> lexIgnoreSome *> path)
  where
    path = (<>) <$> part <*> (concat <$> (many $ (:) <$> char '.' <*> part))
    part = some $ oneOf alphanum
