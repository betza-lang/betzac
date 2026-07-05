module Betzac.Debug.PrettyPrint (PrettyPrint (..), Summarizable (..)) where

import Betzac.AST.Types

class PrettyPrint x where
    prettyPrint :: x -> String

class Summarizable x where
    summarize :: x -> String

-- Instances

instance Summarizable (QualifiedStmt p) where
    summarize (Override _ _) = "Override :: QualifiedStmt"
    summarize (Plain _ _) = "Plain :: QualifiedStmt"

instance Summarizable (Directive p) where
    summarize (Using _ _) = "Using :: Directive"
    summarize (Export _ _) = "Export :: Directive"
    summarize (Bare _ _) = "Bare :: Directive"

instance Summarizable (BetzaStmt p) where
    summarize (Assign _ _ _) = "Assign :: BetzaStmt"
    summarize (Anonymous _ _) = "Anonymous :: BetzaStmt"

instance Summarizable (BetzaExpr p) where
    summarize = const "BetzaExpr"

instance Summarizable (ChainExpr p) where
    summarize = const "ChainExpr"

instance Summarizable (ChainLeg p) where
    summarize = const "ChainLeg"

instance Summarizable (UnionExpr p) where
    summarize = const "UnionExpr"

instance Summarizable (ModifierExpr p) where
    summarize (ModifierExpr s _ _ _) = "ModifierExpr -- setup=" ++ show s

instance Summarizable (ExponentExpr p) where
    summarize = const "ExponentExpr"

instance Summarizable (AtomExpr p) where
    summarize (Paren _ _) = "Paren :: AtomExpr"
    summarize (From _ _) = "From :: AtomExpr"

instance Summarizable (ChainOperator p) where
    summarize = const "ChainOperator"

instance Summarizable (ChainKind p) where
    summarize (Step _) = "Step"
    summarize (Sequence _) = "Sequence"

instance Summarizable (ChainModality p) where
    summarize (Mandatory _) = "Mandatory"
    summarize (Choose _) = "Choose"
    summarize (IffUnblocked _) = "IffUnblocked"

instance Summarizable (Modifier p) where
    summarize (Directional _ _) = "Directional :: Modifier"
    summarize (Behavioural _ _) = "Behavioural :: Modifier"

instance Summarizable (DirectionModifier p) where
    summarize (Amalgamated _ _ _) = "Amalgamated :: DirectionModifier"
    summarize (Single _ _) = "Single :: DirectionModifier"

instance Summarizable (Direction p) where
    summarize (Forward _) = "Forward"
    summarize (Backward _) = "Backward"
    summarize (Leftward _) = "Leftward"
    summarize (Rightward _) = "Rightward"
    summarize (Sideway _) = "Sideway"
    summarize (Vertically _) = "Vertically"
    summarize (All _) = "All"

instance Summarizable (Behaviour p) where
    summarize = const "Behaviour"

instance Summarizable (BehaviourKind p) where
    summarize (Capture _) = "Capture"
    summarize (Leap _) = "Leap"
    summarize (Initial _) = "Initial"
    summarize (Jump _) = "Jump"
    summarize (Move _) = "Move"
    summarize (NoJump _) = "NoJump"
    summarize (Hop _) = "Hop"

instance Summarizable (BehaviourModality p) where
    summarize (Once _) = "Once"
    summarize (Twice _) = "Twice"
    summarize (Any _) = "Any"

instance Summarizable (Exponent p) where
    summarize = const "Exponent"

instance Summarizable (ExponentKind p) where
    summarize (Infinite _) = "Infinite"
    summarize (Slippery _) = "Slippery"
    summarize (Repeat _ _) = "Repeat"

instance Summarizable (Label p) where
    summarize (Upper _ _) = "Upper :: Label"
    summarize (Descriptor _ _) = "Descriptor :: Label"
    summarize (Leaper _ _ _) = "Leaper :: Label"
