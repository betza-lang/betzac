{-# LANGUAGE InstanceSigs #-}

module Betzac.Parser.Core (
    module Control.Applicative,
    Parser,
    ParseError (..),
    runParser,
    try,
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
    Left err ->
        Left
            err
                { parseErrSpan = lookupSpan tmap (parseErrTokenIdx err)
                }

data ParseError = ParseError
    { parseErrCommitPos :: Int -- used for commitment decisions
    , parseErrTokenIdx :: Int -- furthest token reached, for reporting
    , parseErrSpan :: Maybe (Int, Int) -- char span of furthest token
    , parseErrToken :: Maybe Token
    }
    deriving (Eq, Show)

instance StreamError ParseError where
    errorAt n = ParseError n n Nothing Nothing
    errorPos = parseErrCommitPos
    resetPos n e = e{parseErrCommitPos = n}
    furthest e f = if parseErrTokenIdx e > parseErrTokenIdx f then e else f

tok :: Token -> Parser Token
tok = matchOne

dispatch :: (Token -> Maybe a) -> Parser a
dispatch f = do
    n <- StreamMutator $ gets fst
    t <- advance
    case f t of
        Just a -> return a
        Nothing ->
            StreamMutator $ lift $ Left $ ParseError n n Nothing (Just t)
