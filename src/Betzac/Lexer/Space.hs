module Betzac.Lexer.Space (lexIgnore, lexIgnoreSome) where

import Betzac.Lexer.Core
import Control.Applicative (empty)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

lexIgnore :: Lexer ()
lexIgnore = L.space space1 (L.skipLineComment "#") empty

lexIgnoreSome :: Lexer ()
lexIgnoreSome = space1 *> lexIgnore
