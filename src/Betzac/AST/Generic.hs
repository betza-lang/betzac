{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Betzac.AST.Generic (universeOf, visit, Walk (..), WalkField, GWalk (..)) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Typeable (Typeable, cast)
import GHC.Generics

import Betzac.Span (Span)

-- | Collect every subterm of type b reachable from x, top-down (including x itself).
universeOf :: forall a b. (WalkField a, Typeable b) => a -> [b]
universeOf x = visit x []

{- | 'x' itself when it is a b, then whatever its children yield. Kept apart from
'walk' so no instance has to remember to offer itself.
-}
visit :: forall a b. (WalkField a, Typeable b) => a -> [b] -> [b]
visit x acc = case cast x of
    Just v -> v : walk x acc
    Nothing -> walk x acc
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
    -- | Every b among x's children, prepended to the accumulator.
    walk :: (Typeable b) => a -> [b] -> [b]
    default walk :: (Generic a, GWalk (Rep a), Typeable b) => a -> [b] -> [b]
    walk = gwalk . from
    {-# INLINE walk #-}

-- Leaves: nothing inside them is ever an AST subterm.

instance Walk Char where
    walk _ acc = acc

instance Walk Int where
    walk _ acc = acc

instance Walk Bool where
    walk _ acc = acc

instance Walk () where
    walk _ acc = acc

-- A span holds no AST subterm, and its 'SourcePos' carries the file name as a
-- 'String', which descending would visit one character at a time, at every node.
instance Walk Span where
    walk _ acc = acc

-- Containers: transparent, so an element is reached as if the container weren't there.

instance (WalkField a) => Walk [a] where
    walk xs acc = foldr visit acc xs

instance (WalkField a) => Walk (Maybe a) where
    walk Nothing acc = acc
    walk (Just x) acc = visit x acc

instance (WalkField a) => Walk (NonEmpty a) where
    walk xs acc = foldr visit acc (NE.toList xs)

-- | The generic representation's half of 'walk': visit each field, left to right.
class GWalk f where
    gwalk :: (Typeable b) => f x -> [b] -> [b]

instance GWalk V1 where
    gwalk v = case v of {}
    {-# INLINE gwalk #-}

instance GWalk U1 where
    gwalk _ acc = acc
    {-# INLINE gwalk #-}

instance (GWalk f, GWalk g) => GWalk (f :*: g) where
    gwalk (a :*: b) acc = gwalk a (gwalk b acc)
    {-# INLINE gwalk #-}

instance (GWalk f, GWalk g) => GWalk (f :+: g) where
    gwalk (L1 a) = gwalk a
    gwalk (R1 b) = gwalk b
    {-# INLINE gwalk #-}

instance (GWalk f) => GWalk (M1 i m f) where
    gwalk (M1 a) = gwalk a
    {-# INLINE gwalk #-}

instance (WalkField c) => GWalk (K1 i c) where
    gwalk (K1 c) = visit c
    {-# INLINE gwalk #-}
