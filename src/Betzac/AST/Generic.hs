{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Betzac.AST.Generic (universeOf) where

import Data.Data (Data, Typeable, cast, gmapQr)

import Betzac.Span (Span)

-- | Collect every subterm of type b reachable from a, top-down (including x itself).
universeOf :: forall a b. (Data a, Typeable b) => a -> [b]
universeOf x = go x []
  where
    go :: forall c. (Data c) => c -> [b] -> [b]
    go y acc = case cast y of
        Just v -> v : rest
        Nothing -> rest
      where
        rest
            | opaque y = acc
            | otherwise = gmapQr ($) acc go y

    {- A span holds no AST subterm, and its 'SourcePos' carries the file name as
    a 'String', which a generic walk would otherwise visit one character at a time,
    once per node of the tree. Descending into them cost 8x the allocation of the
    whole compilation. -}
    opaque :: forall c. (Data c) => c -> Bool
    opaque y = case (cast y :: Maybe Span) of
        Just _ -> True
        Nothing -> False
