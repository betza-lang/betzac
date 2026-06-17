module Betzac.Parser.Core (
    module Control.Applicative,
    Parser,
    ParseError (..),
    runParser,
    peek,
    advance,
    tok,
    dispatch,
    optional,
    someNE,
) where

import Betzac.StreamMutator
import Betzac.Token (Token)
import Control.Applicative (Alternative (..))

type Parser = StreamMutator ParseError Token

runParser :: Parser a -> [Token] -> Either ParseError (a, Int, [Token])
runParser = runMutator

data ParseError = ParseError deriving (Eq, Show)

instance StreamError ParseError where
    errorAt = const ParseError

tok :: Token -> Parser Token
tok = matchOne

dispatch :: (Token -> Maybe a) -> Parser a
dispatch f = do
    t <- advance
    case f t of
        Just a -> return a
        Nothing -> empty
