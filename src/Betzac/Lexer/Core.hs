module Betzac.Lexer.Core (
    module Control.Applicative,
    Lexer,
    LexError (..),
    runLexer,
    peek,
    advance,
    sat,
    failOn,
    char,
    oneOf,
    noneOf,
    match,
    singleton,
) where

-- for peek
import Betzac.StreamMutator
import Control.Applicative (Alternative (..))

type Lexer = StreamMutator LexError Char

runLexer :: Lexer a -> String -> Either LexError (a, Int, String)
runLexer = runMutator

data LexError = LexError Int deriving (Eq, Show)

instance StreamError LexError where
    errorAt = LexError

char :: Char -> Lexer Char
char = matchOne
