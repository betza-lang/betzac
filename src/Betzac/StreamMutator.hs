{-# LANGUAGE DerivingVia #-}

module Betzac.StreamMutator (
    StreamMutator (..),
    runMutator,
    module Control.Applicative,
    StreamError (..),
    peek,
    sat,
    advance,
    failOn,
    matchOne,
    match,
    oneOf,
    noneOf,
    singleton,
    optional,
    someNE,
) where

-- for peek
import Control.Applicative (Alternative (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT (..), get, gets, put)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (listToMaybe)

newtype StreamMutator e s a = StreamMutator (StateT (Int, [s]) (Either e) a)
    deriving
        (Functor, Applicative, Monad)
        via StateT (Int, [s]) (Either e)

runMutator :: StreamMutator e s a -> [s] -> Either e (a, Int, [s])
runMutator (StreamMutator m) ss = (\(a, (n, ss')) -> (a, n, ss')) <$> runStateT m (0, ss)

class StreamError e where
    errorAt :: Int -> e

instance (StreamError e) => Alternative (StreamMutator e s) where
    empty = StreamMutator $ gets fst >>= lift . Left . errorAt
    StreamMutator l <|> StreamMutator r = StreamMutator $ StateT $ \s -> either (const $ runStateT r s) Right (runStateT l s)
    some p = p >>= \x -> (x :) <$> many p

peek :: StreamMutator e s (Maybe s)
peek = StreamMutator $ gets $ listToMaybe . snd

sat :: (StreamError e) => (s -> Bool) -> StreamMutator e s s
sat p = do
    (n, s) <- StreamMutator get
    case s of
        [] -> empty
        (c : cs) -> if p c then StreamMutator $ put (n + 1, cs) >> return c else empty

advance :: (StreamError e) => StreamMutator e s s
advance = sat $ const True

failOn :: (StreamError e) => (s -> Bool) -> StreamMutator e s ()
failOn p = do
    mc <- peek
    case mc of
        Just c | p c -> empty
        _ -> return ()

matchOne :: (Eq s, StreamError e) => s -> StreamMutator e s s
matchOne s = sat (== s)

match :: (Eq s, StreamError e) => [s] -> StreamMutator e s [s]
match = mapM matchOne

oneOf :: (Eq s, StreamError e) => [s] -> StreamMutator e s s
oneOf ss = sat $ (`elem` ss)

noneOf :: (Eq s, StreamError e) => [s] -> StreamMutator e s s
noneOf ss = sat (`notElem` ss)

singleton :: StreamMutator e s s -> StreamMutator e s [s]
singleton = fmap (: [])

optional :: (StreamError e) => StreamMutator e s a -> StreamMutator e s (Maybe a)
optional m = Just <$> m <|> return Nothing

someNE :: (StreamError e) => StreamMutator e s a -> StreamMutator e s (NonEmpty a)
someNE p = (:|) <$> p <*> many p
