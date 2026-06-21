module Betzac.Lexer.ErrorHandling (spanned, buildMap, emptyTokenMap) where

import Betzac.Lexer.Core
import Betzac.StreamMutator
import Control.Monad.Trans.State.Strict (gets)
import GHC.Arr (listArray)

spanned :: Lexer a -> Lexer (a, (Int, Int))
spanned p = do
    start <- currentOffset
    a <- p
    end <- currentOffset
    return (a, (start, end))

currentOffset :: Lexer Int
currentOffset = StreamMutator $ gets fst

buildMap :: [(Int, Int)] -> TokenMap
buildMap spans = TokenMap $ listArray (0, length spans - 1) spans

emptyTokenMap :: TokenMap
emptyTokenMap = TokenMap $ listArray (0, -1) []
