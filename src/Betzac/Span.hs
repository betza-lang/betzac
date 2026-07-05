module Betzac.Span (Span (..), HasSpan (..)) where

import Text.Megaparsec (SourcePos)

data Span
    = RealSpan SourcePos SourcePos
    | Generated
    deriving (Eq, Show)

-- For uniform error reporting
class HasSpan x where
    getSpan :: x -> Span
