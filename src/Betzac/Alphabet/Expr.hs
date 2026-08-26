module Betzac.Alphabet.Expr (
    space,
    whitespace,
    upper,
    alpha,
    nonzeroDigit,
    digit,
    alphanum,
    symbol,
    exprAlphabet,
    direction,
    behaviour,
    isUpper,
    isAlphanum,
    isDirection,
    isBehaviour,
    isWhitespace,
    isDescriptorChar,
)
where

import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (Word)

space :: Char
space = ' '

whitespace :: String
whitespace = space : "\n\t\r\f\b"

upper :: String
upper = ['A' .. 'Z']

alpha :: String
alpha = upper ++ ['a' .. 'z']

nonzeroDigit :: String
nonzeroDigit = ['1' .. '9']

direction :: String
direction = "fblrsva"

behaviour :: String
behaviour = "cgijmnpy"

digit :: String
digit = '0' : nonzeroDigit

alphanum :: String
alphanum = alpha ++ digit

symbol :: String
symbol = "(,)<>-[]{}*:+?!"

exprAlphabet :: String
exprAlphabet = alphanum ++ symbol

{- | Membership in the alphabets above. Each is a set built once, since a lexer or
parser asks these of every character it looks at, and the lists run to 62 entries
with the digits last.
-}
isUpper, isAlphanum, isDirection, isBehaviour, isWhitespace, isDescriptorChar :: Char -> Bool
isUpper c = Set.member c upperSet
isAlphanum c = Set.member c alphanumSet
isDirection c = Set.member c directionSet
isBehaviour c = Set.member c behaviourSet
isWhitespace c = Set.member c whitespaceSet
isDescriptorChar c = Set.member c descriptorSet

upperSet, alphanumSet, directionSet, behaviourSet, whitespaceSet, descriptorSet :: Set Char
upperSet = Set.fromList upper
alphanumSet = Set.fromList alphanum
directionSet = Set.fromList direction
behaviourSet = Set.fromList behaviour
whitespaceSet = Set.fromList whitespace
descriptorSet = Set.fromList (',' : space : alphanum)
