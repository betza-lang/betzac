{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Betzac.AST.Generic (universeOf) where

import Data.Data (Data, Typeable, cast, gmapQ)

-- Collect every subterm of type b reachable from a, top-down (including x itself)
universeOf :: forall a b. (Data a, Typeable b) => a -> [b]
universeOf x = maybe [] (: []) (cast x) ++ concat (gmapQ universeOf x)
