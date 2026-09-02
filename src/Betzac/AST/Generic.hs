{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Betzac.AST.Generic (universeOf, universeBy, Collector (..), visit, Walk (..), WalkField, GWalk (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Typeable (Typeable, cast)
import Data.Void (Void, absurd)
import GHC.Generics

import Betzac.Span (Span)

{- | What a walk is collecting: each node in turn, prepending whatever it contributes.
One walk can therefore gather several unrelated types at once, where one 'universeOf'
per type would descend the whole tree once per type.
-}
newtype Collector r = Collector (forall x. (Typeable x) => x -> [r] -> [r])

-- | Collect every subterm of type b reachable from x, top-down (including x itself).
universeOf :: forall a b. (WalkField a, Typeable b) => a -> [b]
universeOf = universeBy (Collector (\x acc -> maybe acc (: acc) (cast x)))
{-# INLINE universeOf #-}

-- | Everything 'collector' recognises anywhere in x, top-down, in one descent.
universeBy :: forall a r. (WalkField a) => Collector r -> a -> [r]
universeBy collector x = visit collector x []
{-# INLINE universeBy #-}

{- | What 'x' itself contributes, then whatever its children yield. Kept apart from
'walk' so no instance has to remember to offer itself.
-}
visit :: forall a r. (WalkField a) => Collector r -> a -> [r] -> [r]
visit collector@(Collector collect) x acc = collect x (walk collector x acc)
{-# INLINE visit #-}

-- | What a walk needs of every type it can reach: how to descend, and how to recognise.
type WalkField a = (Walk a, Typeable a)

{- | How a walk descends through a type. The instances are the whitelist: a type whose
'walk' ignores its argument ends the descent, and one left out entirely is a compile
error rather than a silently missed subterm.

Deriving 'Generic' and giving an empty instance is the whole obligation for an AST
node; the structure is then resolved at compile time, with none of the runtime
dictionary traffic 'Data.Data.gfoldl' pays at every node.
-}
class Walk a where
    -- | Everything the collector recognises among x's children, prepended to the accumulator.
    walk :: Collector r -> a -> [r] -> [r]
    default walk :: (Generic a, GWalk (Rep a)) => Collector r -> a -> [r] -> [r]
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

-- Leaves: nothing inside them is ever an AST subterm.

instance Walk Char where
    walk _ _ acc = acc

instance Walk Int where
    walk _ _ acc = acc

instance Walk Bool where
    walk _ _ acc = acc

instance Walk () where
    walk _ _ acc = acc

-- A span holds no AST subterm, and its 'SourcePos' carries the file name as a
-- 'String', which descending would visit one character at a time, at every node.
instance Walk Span where
    walk _ _ acc = acc

-- The field of a constructor the phase cannot spell, so the walk never arrives.
instance Walk Void where
    walk _ v _ = absurd v

-- Containers: transparent, so an element is reached as if the container weren't there.

instance (WalkField a) => Walk [a] where
    walk collector xs acc = foldr (visit collector) acc xs
    {-# INLINE walk #-}

instance (WalkField a) => Walk (Maybe a) where
    walk _ Nothing acc = acc
    walk collector (Just x) acc = visit collector x acc
    {-# INLINE walk #-}

instance (WalkField a) => Walk (NonEmpty a) where
    walk collector xs acc = foldr (visit collector) acc (NE.toList xs)
    {-# INLINE walk #-}

-- | The generic representation's half of 'walk': visit each field, left to right.
class GWalk f where
    gwalk :: Collector r -> f x -> [r] -> [r]

instance GWalk V1 where
    gwalk _ v = case v of {}
    {-# INLINE gwalk #-}

instance GWalk U1 where
    gwalk _ _ acc = acc
    {-# INLINE gwalk #-}

instance (GWalk f, GWalk g) => GWalk (f :*: g) where
    gwalk collector (a :*: b) acc = gwalk collector a (gwalk collector b acc)
    {-# INLINE gwalk #-}

instance (GWalk f, GWalk g) => GWalk (f :+: g) where
    gwalk collector (L1 a) = gwalk collector a
    gwalk collector (R1 b) = gwalk collector b
    {-# INLINE gwalk #-}

instance (GWalk f) => GWalk (M1 i m f) where
    gwalk collector (M1 a) = gwalk collector a
    {-# INLINE gwalk #-}

instance (WalkField c) => GWalk (K1 i c) where
    gwalk collector (K1 c) = visit collector c
    {-# INLINE gwalk #-}
