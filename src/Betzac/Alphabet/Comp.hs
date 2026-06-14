module Betzac.Alphabet.Comp (
    pathSep,
    compAlphabet,
) where

import Betzac.Alphabet.Stmt (stmtAlphabet)

pathSep :: Char
pathSep = '.'

commentMarker :: Char
commentMarker = '#'

compAlphabet :: String
compAlphabet = commentMarker : pathSep : stmtAlphabet
