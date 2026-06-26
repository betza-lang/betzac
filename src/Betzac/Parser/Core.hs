module Betzac.Parser.Core (Parser, tok, dispatch) where

import Betzac.Located
import Betzac.Parser.BetzaTokenStream
import qualified Betzac.Token as B

import Data.Void
import Text.Megaparsec

type Parser = Parsec Void BetzaTokenStream

{-# INLINE tok #-}
tok :: B.Token -> Parser (Located B.Token)
tok t = satisfy (\lt -> tokenVal lt == t) <?> B.showToken t

{-# INLINE dispatch #-}
dispatch :: (B.Token -> Maybe a) -> Parser a
dispatch f = token (f . tokenVal) mempty
