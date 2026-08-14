{-# LANGUAGE KindSignatures #-}

module Betzac.AST.Strip (Extensible (..)) where

import Betzac.AST.Phases (Stripped)
import Betzac.AST.Types
import Data.Kind (Type)
import qualified Data.List.NonEmpty as NE

class (Eq (x Stripped)) => Extensible (x :: Type -> Type) where
    strip :: x p -> x Stripped
    stripEq :: x p -> x p -> Bool
    stripEq a b = strip a == strip b

instance Extensible QualifiedStmt where
    strip (Override d _) = Override (strip d) ()
    strip (Plain d _) = Plain (strip d) ()

instance Extensible Directive where
    strip (Using f _) = Using f ()
    strip (Export s _) = Export (strip s) ()
    strip (Bare s _) = Bare (strip s) ()

instance Extensible BetzaStmt where
    strip (Assign l e _) = Assign (strip l) (strip e) ()
    strip (LabelRef l _) = LabelRef (strip l) ()

instance Extensible BetzaExpr where
    strip (BetzaExpr c _) = BetzaExpr (strip c) ()

instance Extensible ChainExpr where
    strip (ChainExpr u mcl _) = ChainExpr (strip u) (maybe Nothing (Just . strip) $ mcl) ()

instance Extensible ChainLeg where
    strip (ChainLeg op c _) = ChainLeg (strip op) (strip c) ()

instance Extensible UnionExpr where
    strip (UnionExpr ms _) = UnionExpr (NE.map strip ms) ()

instance Extensible ModifierExpr where
    strip (ModifierExpr s ms e _) = ModifierExpr s (map strip ms) (strip e) ()

instance Extensible ExponentExpr where
    strip (ExponentExpr a me _) = ExponentExpr (strip a) (maybe Nothing (Just . strip) $ me) ()

instance Extensible AtomExpr where
    strip (Paren e _) = Paren (strip e) ()
    strip (From l _) = From (strip l) ()

instance Extensible ChainOperator where
    strip (ChainOperator k m _) = ChainOperator (strip k) (strip m) ()

instance Extensible ChainKind where
    strip (Step _) = Step ()
    strip (Sequence _) = Sequence ()

instance Extensible ChainModality where
    strip (Mandatory _) = Mandatory ()
    strip (Choose _) = Choose ()
    strip (IffUnblocked _) = IffUnblocked ()

instance Extensible Modifier where
    strip (Directional d _) = Directional (strip d) ()
    strip (Behavioural b _) = Behavioural (strip b) ()

instance Extensible DirectionModifier where
    strip (Amalgamated x y _) = Amalgamated (strip x) (strip y) ()
    strip (Single d _) = Single (strip d) ()

instance Extensible Direction where
    strip (Forward _) = Forward ()
    strip (Backward _) = Backward ()
    strip (Leftward _) = Leftward ()
    strip (Rightward _) = Rightward ()
    strip (Sideway _) = Sideway ()
    strip (Vertically _) = Vertically ()
    strip (All _) = All ()

instance Extensible Behaviour where
    strip (Behaviour k m _) = Behaviour (strip k) (strip m) ()

instance Extensible BehaviourKind where
    strip (Capture _) = Capture ()
    strip (Leap _) = Leap ()
    strip (Initial _) = Initial ()
    strip (Jump _) = Jump ()
    strip (Move _) = Move ()
    strip (NoJump _) = NoJump ()
    strip (Hop _) = Hop ()

instance Extensible BehaviourModality where
    strip (Once _) = Once ()
    strip (Twice _) = Twice ()
    strip (Any _) = Any ()

instance Extensible Exponent where
    strip (Exponent mco ms k _) =
        Exponent
            (maybe Nothing (Just . strip) $ mco)
            (map strip ms)
            (strip k)
            ()

instance Extensible ExponentKind where
    strip (Infinite _) = Infinite ()
    strip (Slippery _) = Slippery ()
    strip (Repeat n _) = Repeat n ()

instance Extensible Label where
    strip (Upper c _) = Upper c ()
    strip (Descriptor s _) = Descriptor s ()
    strip (Leaper x y _) = Leaper x y ()
