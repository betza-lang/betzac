{-# LANGUAGE FlexibleInstances #-}

module Betzac.Semantic.Modifier.Subsume (analysisSubsumedModifiers) where

import Betzac.AST
import Betzac.Diagnostic
import Betzac.Utils.Helper (groupByEq)

import Control.Monad (when)
import Data.List (inits, tails)
import qualified Data.Set as Set

analysisSubsumedModifiers :: [Modifier Ps] -> Pass ()
analysisSubsumedModifiers ms = analysisSubsumedDirections [m | (Directional m _) <- ms] >> analysisSubsumedBehaviours [m | (Behavioural m _) <- ms]

data CardinalDirection = N | S | E | W
    deriving (Eq, Ord)

data DisplacementClass
    = Axis CardinalDirection
    | Diagonal CardinalDirection CardinalDirection
    | Oblique CardinalDirection CardinalDirection -- first one has larger component

instance Eq DisplacementClass where
    (Diagonal c1 c2) == (Diagonal d1 d2) = (min c1 c2, max c1 c2) == (min d1 d2, max d1 d2)
    (Axis c) == (Axis d) = c == d
    (Oblique c1 c2) == (Oblique d1 d2) = c1 == d1 && c2 == d2
    _ == _ = False

instance Ord DisplacementClass where
    Axis c <= Axis d = c <= d
    Axis _ <= Diagonal _ _ = True
    Axis _ <= Oblique _ _ = True
    Diagonal c1 c2 <= Diagonal d1 d2 = (min c1 c2, max c1 c2) <= (min d1 d2, max d1 d2)
    Diagonal _ _ <= Oblique _ _ = True
    Oblique c1 c2 <= Oblique d1 d2 = (c1, c2) <= (d1, d2)
    _ <= _ = False

{- | Minimal instance: @rotRight@, @reflectX@.

Must satisfy:

@reflectX . rotRight === rotLeft . reflectX@

@rotRight . reflectY === reflectX . rotRight@
-}
class Dihedral a where
    rotRight :: a -> a
    rotLeft :: a -> a
    rotLeft = reflectX . rotRight . reflectX

    -- | Reflect the x coordinate
    reflectX :: a -> a

    -- | Reflect the y coordinate
    reflectY :: a -> a
    reflectY = rotLeft . reflectX . rotRight

instance Dihedral CardinalDirection where
    rotRight N = E
    rotRight E = S
    rotRight S = W
    rotRight W = N
    reflectX N = N
    reflectX E = W
    reflectX S = S
    reflectX W = E

instance Dihedral DisplacementClass where
    rotRight (Axis c) = Axis (rotRight c)
    rotRight (Diagonal c1 c2) = Diagonal (rotRight c1) (rotRight c2)
    rotRight (Oblique c1 c2) = Oblique (rotRight c1) (rotRight c2)
    reflectX (Axis c) = Axis (reflectX c)
    reflectX (Diagonal c1 c2) = Diagonal (reflectX c1) (reflectX c2)
    reflectX (Oblique c1 c2) = Oblique (reflectX c1) (reflectX c2)

instance Dihedral (Direction Stripped) where
    rotRight d@(All _) = d
    rotRight (Forward ()) = Rightward ()
    rotRight (Rightward ()) = Backward ()
    rotRight (Backward ()) = Leftward ()
    rotRight (Leftward ()) = Forward ()
    rotRight (Sideway ()) = Vertically ()
    rotRight (Vertically ()) = Sideway ()
    reflectX (Rightward ()) = Leftward ()
    reflectX (Leftward ()) = Rightward ()
    reflectX d = d

-- | One representative per shape, turned through all four quarters.
allClasses :: [DisplacementClass]
allClasses = concatMap quarter [Axis N, Diagonal N E, Oblique N E, Oblique E N]
  where
    quarter = take 4 . iterate rotRight

forwardClasses :: [DisplacementClass]
forwardClasses =
    [ Axis N
    , Diagonal N E
    , Diagonal N W
    , Oblique N E
    , Oblique E N
    , Oblique W N
    , Oblique N W
    ]

{- | A doubled modifier constrains the greater component and leaves the lesser free. An
axis displacement qualifies -- its zero component is exactly what nothing constrains --
so @<ff>W@ selects what @fW@ selects. A diagonal does not: it has no greater component.
-}
forwardTwiceClasses :: [DisplacementClass]
forwardTwiceClasses =
    [ Axis N
    , Oblique N E
    , Oblique N W
    ]

forwardRightClasses :: [DisplacementClass]
forwardRightClasses =
    [ Diagonal N E
    , Oblique N E
    ]

classesOf :: (Qualifying a) => DirectionModifier a -> Set.Set DisplacementClass
classesOf dm = Set.fromList . classesOf' . (strip <$>) $ case dm of
    Single d _ -> [d]
    Amalgamated d1 d2 _ -> [d1, d2]
  where
    classesOf' :: [Direction Stripped] -> [DisplacementClass]
    classesOf' [All _] = allClasses
    classesOf' [Forward _] = forwardClasses
    classesOf' [Rightward _] = rotRight <$> forwardClasses
    classesOf' [Backward _] = reflectY <$> forwardClasses
    classesOf' [Leftward _] = rotLeft <$> forwardClasses
    classesOf' [Vertically _] = classesOf' [Forward ()] ++ classesOf' [Backward ()]
    classesOf' [Sideway _] = classesOf' [Leftward ()] ++ classesOf' [Rightward ()]
    classesOf' [Forward _, Forward _] = forwardTwiceClasses
    classesOf' [Rightward _, Rightward _] = rotRight <$> forwardTwiceClasses
    classesOf' [Backward _, Backward _] = reflectY <$> forwardTwiceClasses
    classesOf' [Leftward _, Leftward _] = rotLeft <$> forwardTwiceClasses
    classesOf' [Forward _, Rightward _] = forwardRightClasses
    classesOf' [Forward _, Leftward _] = reflectX <$> forwardRightClasses
    classesOf' ds@[Leftward _, _] = rotLeft <$> (classesOf' $ rotRight <$> ds)
    classesOf' ds@[Rightward _, _] = rotRight <$> (classesOf' $ rotLeft <$> ds)
    classesOf' ds@[Backward _, _] = rotLeft . rotLeft <$> (classesOf' $ rotRight . rotRight <$> ds)
    classesOf' _ = []

analysisSubsumedDirections :: [DirectionModifier Ps] -> Pass ()
analysisSubsumedDirections ms =
    let classes = zip ms $ map classesOfHead $ groupByEq stripEq ms
        n = length classes
        runs = map (take n) . take n $ tails . cycle $ classes
     in mapM_ go runs
  where
    classesOfHead :: [DirectionModifier Ps] -> Set.Set DisplacementClass
    classesOfHead (dm : _) = classesOf dm
    classesOfHead _ = Set.empty

    go :: [(DirectionModifier Ps, Set.Set DisplacementClass)] -> Pass ()
    go ((m, d) : mds) = when (d `Set.isSubsetOf` Set.unions (map snd mds)) $ emitRedundant m
    go _ = pure ()

    emitRedundant :: DirectionModifier Ps -> Pass ()
    emitRedundant = emitWarningAt $ RedundantModifier "subsumed by nearby modifiers"

-- | Whether the first behaviour selects everything the second selects, and strictly more.
subsumes :: (Qualifying a) => Behaviour a -> Behaviour a -> Bool
Behaviour _ (Any _) _ `subsumes` Behaviour _ (Any _) _ = False
Behaviour kind1 (Any _) _ `subsumes` Behaviour kind2 _ _ = kind1 `stripEq` kind2
_ `subsumes` _ = False

analysisSubsumedBehaviours :: [Behaviour Ps] -> Pass ()
analysisSubsumedBehaviours ms =
    mapM_ (emitWarningAt $ RedundantModifier "subsumed by nearby modifiers") $
        [ m
        | (m, before, after) <- zip3 ms (inits ms) (drop 1 $ tails ms)
        , any (`subsumes` m) (before ++ after)
        ]
