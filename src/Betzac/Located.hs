module Betzac.Located (Located (..), liftLocated, located) where

import Text.Megaparsec (MonadParsec, SourcePos, TraversableStream, getSourcePos, initialPos)

data Located a = Located
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenVal :: a
    }
    deriving (Eq, Ord, Show)

instance Functor Located where
    fmap f (Located s e a) = Located s e (f a)

liftLocated :: FilePath -> a -> Located a
liftLocated f a = Located pos pos a
  where
    pos = initialPos f

located :: (TraversableStream s, MonadParsec e s m) => m a -> m (Located a)
located p = do
    start <- getSourcePos
    a <- p
    end <- getSourcePos
    return $ Located start end a
