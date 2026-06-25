module Betzac.Located (Located (..), liftLocated) where

import Text.Megaparsec (SourcePos, initialPos)

data Located a = Located
    { startPos :: SourcePos
    , endPos :: SourcePos
    , tokenLength :: Int
    , tokenVal :: a
    }
    deriving (Eq, Ord, Show)

liftLocated :: FilePath -> a -> Located a
liftLocated f a = Located pos pos 0 a
  where
    pos = initialPos f
