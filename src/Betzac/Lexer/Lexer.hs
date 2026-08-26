{-# LANGUAGE OverloadedStrings #-}

module Betzac.Lexer.Lexer (lexSource, runLexer) where

import qualified Betzac.Alphabet.Expr as B
import qualified Betzac.Alphabet.Stmt as B
import Betzac.Located (Located (..), tokenVal)
import qualified Betzac.Token as B

import Betzac.Lexer.Core
import Betzac.Lexer.Space

import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char.Lexer as L

{- | Best-effort: tokens up to and around any failure are always returned. A
failure inside one token is recovered by skipping to the next ';' (or eof) —
see 'recoverableToken' — so the accumulated errors (if any) come back
alongside the tokens rather than short-circuiting them. Errors are threaded
through as plain 'Either' values rather than via megaparsec's own
'registerParseError'/'stateParseErrors' machinery: registering an error from
inside a recovered branch, even though documented as non-failing, was
observed to make the *whole* parse fail outright once wrapped in 'manyTill' —
so accumulation is done by hand here instead.
-}
runLexer :: FilePath -> String -> ([Located B.Token], Maybe (ParseErrorBundle String Void))
runLexer f src = case parse lexSource f src of
    Left bundle -> ([], Just bundle)
    Right results ->
        let (errs, toks) = partitionEithers results
         in (toks, toErrorBundle f src errs)

toErrorBundle :: FilePath -> String -> [ParseError String Void] -> Maybe (ParseErrorBundle String Void)
toErrorBundle f src errs = case NE.nonEmpty errs of
    Nothing -> Nothing
    Just ne -> Just $ ParseErrorBundle (NE.sortWith errorOffset ne) initPosState
  where
    initPosState =
        PosState
            { pstateInput = src
            , pstateOffset = 0
            , pstateSourcePos = initialPos f
            , pstateTabWidth = defaultTabWidth
            , pstateLinePrefix = ""
            }

{- | One token, best-effort: 'Right' on success; on failure, 'Left' the error
after skipping input up to (but not including) the next ';' -- *unless*
'hasContent' says nothing has been lexed yet for the statement currently in
progress, in which case there's nothing worth preserving and the ';' is
consumed too (matching what 'recoverableStmt' in the parser would otherwise
have to reject on its own as an empty statement, doubling up on the same
underlying typo).
-}
recoverableToken :: Bool -> Lexer (Either (ParseError String Void) (Located B.Token))
recoverableToken hasContent =
    withRecovery
        (\e -> Left e <$ recover)
        (Right <$> (spanned lexToken' <* lexIgnore))
  where
    lexToken' = lexDirective <|> lexToken
    recover = do
        _ <- takeWhileP (Just "recovery: skipped input") (/= B.stmtEnd)
        if hasContent
            then return ()
            else () <$ optional (char B.stmtEnd) <* lexIgnore

lexSource :: Lexer [Either (ParseError String Void) (Located B.Token)]
lexSource = lexIgnore *> go False
  where
    go :: Bool -> Lexer [Either (ParseError String Void) (Located B.Token)]
    go hasContent =
        ([] <$ hidden eof) <|> do
            result <- recoverableToken hasContent
            let hasContent' = case result of
                    Right t -> tokenVal t /= B.TokEndStmt
                    Left _ -> hasContent
            (result :) <$> go hasContent'

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
lexAtom = B.TokAtom <$> satisfy B.isUpper

lexDescriptor :: Lexer B.Token
lexDescriptor = try $ B.TokDescriptor <$> (char ':' *> descriptor <* char ':')
  where
    descriptor = takeWhile1P (Just "descriptor character") B.isDescriptorChar

lexDirection :: Lexer B.Token
lexDirection = B.TokDirection <$> satisfy B.isDirection

lexBehaviour :: Lexer B.Token
lexBehaviour = B.TokBehaviour <$> satisfy B.isBehaviour

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
lexKeyword kw t = t <$ string kw <* notFollowedBy (satisfy B.isAlphanum)

lexOverride :: Lexer B.Token
lexOverride = lexKeyword "override" B.TokOverride

lexExport :: Lexer B.Token
lexExport = lexKeyword "export" B.TokExport

lexUsing :: Lexer B.Token
lexUsing = B.TokUsing <$> (string "using" *> lexIgnoreSome *> path)
  where
    path = (<>) <$> part <*> (concat <$> (many $ try $ (:) <$> char '.' <*> part))
    part = takeWhile1P (Just "path character") B.isAlphanum
