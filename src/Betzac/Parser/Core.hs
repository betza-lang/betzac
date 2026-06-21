{-# LANGUAGE InstanceSigs #-}

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

import Betzac.Lexer.Core (TokenMap, lookupSpan)
import Betzac.StreamMutator
import Betzac.Token (Token)
import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets)

type Parser = StreamMutator ParseError Token

runParser :: TokenMap -> Parser a -> [Token] -> Either ParseError (a, Int, [Token])
runParser tmap p tokens = case runMutator p tokens of
    Right x -> Right x
    Left err -> Left err{parseErrSpan = lookupSpan tmap (parseErrTokenIdx err)}

data ParseError = ParseError
    { parseErrTokenIdx :: Int
    , parseErrSpan :: Maybe (Int, Int)
    , parseErrToken :: Maybe Token
    }
    deriving (Eq, Show)

instance StreamError ParseError where
    errorAt n = ParseError n Nothing Nothing
    errorPos e = parseErrTokenIdx e

tok :: Token -> Parser Token
tok = matchOne

dispatch :: (Token -> Maybe a) -> Parser a
dispatch f = do
    n <- StreamMutator $ gets fst
    t <- advance
    case f t of
        Just a -> return a
        Nothing ->
            StreamMutator $ lift $ Left $ ParseError n Nothing (Just t)
