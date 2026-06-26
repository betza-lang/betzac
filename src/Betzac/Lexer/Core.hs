module Betzac.Lexer.Core (Lexer, spanned) where

import Betzac.Located
import qualified Betzac.Token as B
import Data.Void
import Text.Megaparsec

type Lexer = Parsec Void String

{-# INLINE spanned #-}
spanned :: Lexer B.Token -> Lexer (Located B.Token)
spanned p = do
    start <- getSourcePos
    a <- p
    end <- getSourcePos
    let len = unPos (sourceColumn end) - unPos (sourceColumn start)
    return $ Located start end len a
