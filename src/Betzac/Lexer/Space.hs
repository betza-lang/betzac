module Betzac.Lexer.Space (lexIgnore, lexIgnoreSome) where

import Betzac.Alphabet.Expr (whitespace)
import Betzac.Lexer.Core
import Control.Applicative (empty)
import Text.Megaparsec (takeWhile1P)
import qualified Text.Megaparsec.Char.Lexer as L

-- | One or more whitespace characters from the betza alphabet (spec 3.1) —
-- deliberately narrower than megaparsec's Unicode-aware 'Text.Megaparsec.Char.space1'.
{-# INLINE lexWhitespace #-}
lexWhitespace :: Lexer ()
lexWhitespace = () <$ takeWhile1P (Just "whitespace") (`elem` whitespace)

{-# INLINE lexIgnore #-}
lexIgnore :: Lexer ()
lexIgnore = L.space lexWhitespace (L.skipLineComment "#") empty

{-# INLINE lexIgnoreSome #-}
lexIgnoreSome :: Lexer ()
lexIgnoreSome = lexWhitespace *> lexIgnore
