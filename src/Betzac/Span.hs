{-# LANGUAGE DeriveDataTypeable #-}

module Betzac.Span (Span (..), HasSpan (..)) where

import Data.Data (Data)
import Text.Megaparsec (SourcePos)

data Span
    = RealSpan SourcePos SourcePos
    | Generated
    deriving (Eq, Show, Data)

-- For uniform error reporting
class HasSpan x where
    getSpan :: x -> Span
