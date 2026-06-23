module Betzac.Lexer.Core (
    module Control.Applicative,
    Lexer,
    LexError (..),
    TokenMap (..),
    lookupSpan,
    runLexer,
    try,
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

import Betzac.StreamMutator
import Control.Applicative (Alternative (..))
import GHC.Arr (Array, Ix (inRange), bounds, (!))

type Lexer = StreamMutator LexError Char

runLexer :: Lexer a -> String -> Either LexError (a, Int, String)
runLexer = runMutator

data LexError = LexError Int deriving (Eq, Show)

instance StreamError LexError where
    errorAt = LexError
    errorPos (LexError n) = n

char :: Char -> Lexer Char
char = matchOne

data TokenMap = TokenMap (Array Int (Int, Int))
    deriving (Show)

lookupSpan :: TokenMap -> Int -> Maybe (Int, Int)
lookupSpan (TokenMap arr) i
    | inRange (bounds arr) i = Just (arr ! i)
    | otherwise = Nothing
