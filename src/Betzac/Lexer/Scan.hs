module Betzac.Lexer.Scan (lexIgnore, lexWhitespace, lexComment, lexIgnoreSomeLeadingWhitespace) where

import Betzac.Alphabet.Expr
import Betzac.Lexer.Core

lexComment :: Lexer ()
lexComment = () <$ (char '#' >> (many $ sat (/= '\n')))

lexIgnore :: Lexer ()
lexIgnore = () <$ many (lexWhitespace <|> lexComment)

lexWhitespace :: Lexer ()
lexWhitespace = () <$ some (oneOf whitespace)

lexIgnoreSomeLeadingWhitespace :: Lexer ()
lexIgnoreSomeLeadingWhitespace = (oneOf whitespace) >> lexIgnore
