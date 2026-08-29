module Betzac.Utils.Helper (groupByEq) where

import qualified Data.List as L

groupByEq :: (Eq a) => (a -> a -> Bool) -> [a] -> [[a]]
groupByEq _ [] = []
groupByEq eq (x : xs) =
    let (same, rest) = L.partition (eq x) xs
     in (x : same) : groupByEq eq rest
