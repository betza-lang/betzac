{-# LANGUAGE DerivingVia #-}

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

import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), get, gets, put, runStateT)
import Data.Maybe (listToMaybe) -- for peek

newtype Lexer a = Lexer (StateT (Int, String) (Either LexError) a)
    deriving
        (Functor, Applicative, Monad)
        via StateT (Int, String) (Either LexError)

runLexer :: Lexer a -> String -> Either LexError (a, Int, String)
runLexer (Lexer l) s = (\(a, (n, s')) -> (a, n, s')) <$> runStateT l (0, s)

data LexError = LexError Int deriving (Eq, Show)

instance Alternative Lexer where
    empty = Lexer $ gets fst >>= lift . Left . LexError
    Lexer l <|> Lexer r = Lexer $ StateT $ \s -> either (const $ runStateT r s) Right (runStateT l s)
    some p = p >>= \x -> (x :) <$> many p

peek :: Lexer (Maybe Char)
peek = Lexer $ gets $ listToMaybe . snd

sat :: (Char -> Bool) -> Lexer Char
sat p = do
    (n, s) <- Lexer get
    case s of
        [] -> empty
        (c : cs) -> if p c then Lexer $ put (n + 1, cs) >> return c else empty

advance :: Lexer Char
advance = sat $ const True

failOn :: (Char -> Bool) -> Lexer ()
failOn p = do
    mc <- peek
    case mc of
        Just c | p c -> empty
        _ -> return ()

char :: Char -> Lexer Char
char c = sat (== c)

oneOf :: String -> Lexer Char
oneOf s = sat $ (`elem` s)

noneOf :: String -> Lexer Char
noneOf s = sat (`notElem` s)

match :: String -> Lexer String
match [] = pure []
match (c : cs) = (:) <$> (char c) <*> (match cs)

singleton :: Lexer a -> Lexer [a]
singleton = fmap (: [])
