module Betzac.Located (
    Span (..),
    HasSpan (..),
    Located (..),
    liftLocated,
    located,
) where

import Text.Megaparsec (
    MonadParsec,
    SourcePos,
    TraversableStream,
    getSourcePos,
    initialPos,
 )

data Span
    = RealSpan SourcePos SourcePos
    | Generated
    deriving (Eq, Show)

-- For uniform error reporting
class HasSpan x where
    getSpan :: x -> Span

-- TODO: Located should likely contain a Span
data Located a = Located
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenVal :: a
    }
    deriving (Eq, Ord, Show)

instance HasSpan (Located a) where
    getSpan (Located s e _) = RealSpan s e

instance Functor Located where
    fmap f (Located s e a) = Located s e (f a)

liftLocated :: FilePath -> a -> Located a
liftLocated f a = Located pos pos a
  where
    pos = initialPos f

located :: (TraversableStream s, MonadParsec e s m) => m a -> m (Located a)
located p = do
    s <- getSourcePos
    a <- p
    e <- getSourcePos
    return $ Located s e a