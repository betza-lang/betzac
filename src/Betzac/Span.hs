{-# LANGUAGE DeriveDataTypeable #-}

module Betzac.Span (Span (..), HasSpan (..)) where

import Data.Data (Data)
import Data.Void (Void, absurd)
import Text.Megaparsec (SourcePos)

data Span
    = RealSpan SourcePos SourcePos
    | Generated
    deriving (Eq, Ord, Show, Data)

-- For uniform error reporting
class HasSpan x where
    getSpan :: x -> Span

-- Used for Stripped phase
instance HasSpan () where
    getSpan () = Generated

-- Used to delete constructors, notably in Ds phase
instance HasSpan Void where
    getSpan = absurd
