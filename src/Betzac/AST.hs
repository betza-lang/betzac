{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Betzac.AST where

import Betzac.Located (HasSpan (..), Span (..))

import Betzac.Debug.PrettyPrint (Summarizable (..))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)

-- Phase indices
data Stripped -- for debugging and testing: all extensions are unit type
data Ps -- parsed
-- data Ds -- desugared

-- Global extension records
data PsX = PsX {psSpan :: Span}
    deriving (Eq, Show)

-- Every stage must be able to report errors
instance HasSpan PsX where
    getSpan = psSpan

-- Extension type families, giving these constructors annotations
-- XConstructorName Phase = FieldType
-- Statement/directive level
type family XOverride (p :: Type) :: Type
type family XPlain (p :: Type) :: Type
type family XUsing (p :: Type) :: Type
type family XExport (p :: Type) :: Type
type family XBare (p :: Type) :: Type
type family XAssign (p :: Type) :: Type
type family XAnonymous (p :: Type) :: Type

-- Expression level
type family XBetzaExpr (p :: Type) :: Type
type family XChainExpr (p :: Type) :: Type
type family XChainLeg (p :: Type) :: Type
type family XUnionExpr (p :: Type) :: Type
type family XModifierExpr (p :: Type) :: Type
type family XExponentExpr (p :: Type) :: Type
type family XParen (p :: Type) :: Type
type family XFrom (p :: Type) :: Type

-- Modifier level
type family XChainOperator (p :: Type) :: Type
type family XIffUnblocked (p :: Type) :: Type
type family XDirectional (p :: Type) :: Type
type family XBehavioural (p :: Type) :: Type
type family XAmalgamated (p :: Type) :: Type
type family XSingle (p :: Type) :: Type
type family XBehaviour (p :: Type) :: Type
type family XExponent (p :: Type) :: Type

-- Kinds, modalities, etc.
type family XStep (p :: Type) :: Type
type family XSequence (p :: Type) :: Type
type family XMandatory (p :: Type) :: Type
type family XChoose (p :: Type) :: Type
type family XForward (p :: Type) :: Type
type family XBackward (p :: Type) :: Type
type family XLeftward (p :: Type) :: Type
type family XRightward (p :: Type) :: Type
type family XSideway (p :: Type) :: Type
type family XVertically (p :: Type) :: Type
type family XAll (p :: Type) :: Type
type family XCapture (p :: Type) :: Type
type family XLeap (p :: Type) :: Type
type family XInitial (p :: Type) :: Type
type family XJump (p :: Type) :: Type
type family XMove (p :: Type) :: Type
type family XNoJump (p :: Type) :: Type
type family XHop (p :: Type) :: Type
type family XOnce (p :: Type) :: Type
type family XTwice (p :: Type) :: Type
type family XAny (p :: Type) :: Type
type family XInfinite (p :: Type) :: Type
type family XSlippery (p :: Type) :: Type
type family XRepeat (p :: Type) :: Type
type family XUpper (p :: Type) :: Type
type family XDescriptor (p :: Type) :: Type
type family XLeaper (p :: Type) :: Type

-- Stripped phase instances
type instance XOverride Stripped = ()
type instance XPlain Stripped = ()
type instance XUsing Stripped = ()
type instance XExport Stripped = ()
type instance XBare Stripped = ()
type instance XAssign Stripped = ()
type instance XAnonymous Stripped = ()
type instance XBetzaExpr Stripped = ()
type instance XChainExpr Stripped = ()
type instance XChainLeg Stripped = ()
type instance XUnionExpr Stripped = ()
type instance XModifierExpr Stripped = ()
type instance XExponentExpr Stripped = ()
type instance XParen Stripped = ()
type instance XFrom Stripped = ()
type instance XChainOperator Stripped = ()
type instance XIffUnblocked Stripped = ()
type instance XDirectional Stripped = ()
type instance XBehavioural Stripped = ()
type instance XAmalgamated Stripped = ()
type instance XSingle Stripped = ()
type instance XBehaviour Stripped = ()
type instance XExponent Stripped = ()
type instance XStep Stripped = ()
type instance XSequence Stripped = ()
type instance XMandatory Stripped = ()
type instance XChoose Stripped = ()
type instance XForward Stripped = ()
type instance XBackward Stripped = ()
type instance XLeftward Stripped = ()
type instance XRightward Stripped = ()
type instance XSideway Stripped = ()
type instance XVertically Stripped = ()
type instance XAll Stripped = ()
type instance XCapture Stripped = ()
type instance XLeap Stripped = ()
type instance XInitial Stripped = ()
type instance XJump Stripped = ()
type instance XMove Stripped = ()
type instance XNoJump Stripped = ()
type instance XHop Stripped = ()
type instance XOnce Stripped = ()
type instance XTwice Stripped = ()
type instance XAny Stripped = ()
type instance XInfinite Stripped = ()
type instance XSlippery Stripped = ()
type instance XRepeat Stripped = ()
type instance XUpper Stripped = ()
type instance XDescriptor Stripped = ()
type instance XLeaper Stripped = ()

-- Ps phase instances
type instance XOverride Ps = PsX
type instance XPlain Ps = PsX
type instance XUsing Ps = PsX
type instance XExport Ps = PsX
type instance XBare Ps = PsX
type instance XAssign Ps = PsX
type instance XAnonymous Ps = PsX
type instance XBetzaExpr Ps = PsX
type instance XChainExpr Ps = PsX
type instance XChainLeg Ps = PsX
type instance XUnionExpr Ps = PsX
type instance XModifierExpr Ps = PsX
type instance XExponentExpr Ps = PsX
type instance XParen Ps = PsX
type instance XFrom Ps = PsX
type instance XChainOperator Ps = PsX
type instance XIffUnblocked Ps = PsX
type instance XDirectional Ps = PsX
type instance XBehavioural Ps = PsX
type instance XAmalgamated Ps = PsX
type instance XSingle Ps = PsX
type instance XBehaviour Ps = PsX
type instance XExponent Ps = PsX
type instance XStep Ps = PsX
type instance XSequence Ps = PsX
type instance XMandatory Ps = ()
type instance XChoose Ps = PsX
type instance XForward Ps = PsX
type instance XBackward Ps = PsX
type instance XLeftward Ps = PsX
type instance XRightward Ps = PsX
type instance XSideway Ps = PsX
type instance XVertically Ps = PsX
type instance XAll Ps = PsX
type instance XCapture Ps = PsX
type instance XLeap Ps = PsX
type instance XInitial Ps = PsX
type instance XJump Ps = PsX
type instance XMove Ps = PsX
type instance XNoJump Ps = PsX
type instance XHop Ps = PsX
type instance XOnce Ps = ()
type instance XTwice Ps = PsX
type instance XAny Ps = PsX
type instance XInfinite Ps = PsX
type instance XSlippery Ps = PsX
type instance XRepeat Ps = PsX
type instance XUpper Ps = PsX
type instance XDescriptor Ps = PsX
type instance XLeaper Ps = PsX

-- AST types, parameterized by phase

type BetzaProgram p = [QualifiedStmt p]

data QualifiedStmt p
    = Override (Directive p) (XOverride p)
    | Plain (Directive p) (XPlain p)

data Directive p
    = Using FilePath (XUsing p)
    | Export (BetzaStmt p) (XExport p)
    | Bare (BetzaStmt p) (XBare p)

data BetzaStmt p
    = Assign (Label p) (BetzaExpr p) (XAssign p)
    | Anonymous (BetzaExpr p) (XAnonymous p)

data BetzaExpr p = BetzaExpr (ChainExpr p) (XBetzaExpr p)

data ChainExpr p = ChainExpr (UnionExpr p) (Maybe (ChainLeg p)) (XChainExpr p)

data ChainLeg p = ChainLeg (ChainOperator p) (ChainExpr p) (XChainLeg p)

data UnionExpr p = UnionExpr (NonEmpty (ModifierExpr p)) (XUnionExpr p)

data ModifierExpr p = ModifierExpr
    { setup :: Bool
    , modifiers :: [Modifier p]
    , expExpr :: ExponentExpr p
    , modExprExt :: XModifierExpr p
    }

data ExponentExpr p = ExponentExpr (AtomExpr p) (Maybe (Exponent p)) (XExponentExpr p)

data AtomExpr p
    = Paren (BetzaExpr p) (XParen p)
    | From (Label p) (XFrom p)

data ChainOperator p = ChainOperator (ChainKind p) (ChainModality p) (XChainOperator p)

data ChainKind p
    = Step (XStep p)
    | Sequence (XSequence p)

data ChainModality p
    = Mandatory (XMandatory p)
    | Choose (XChoose p)
    | IffUnblocked (XIffUnblocked p)

data Modifier p
    = Directional (DirectionModifier p) (XDirectional p)
    | Behavioural (Behaviour p) (XBehavioural p)

data DirectionModifier p
    = Amalgamated (Direction p) (Direction p) (XAmalgamated p)
    | Single (Direction p) (XSingle p)

data Direction p
    = Forward (XForward p)
    | Backward (XBackward p)
    | Leftward (XLeftward p)
    | Rightward (XRightward p)
    | Sideway (XSideway p)
    | Vertically (XVertically p)
    | All (XAll p)

data Behaviour p = Behaviour (BehaviourKind p) (BehaviourModality p) (XBehaviour p)

data BehaviourKind p
    = Capture (XCapture p)
    | Leap (XLeap p)
    | Initial (XInitial p)
    | Jump (XJump p)
    | Move (XMove p)
    | NoJump (XNoJump p)
    | Hop (XHop p)

data BehaviourModality p
    = Once (XOnce p)
    | Twice (XTwice p)
    | Any (XAny p)

data Exponent p
    = Exponent
        (Maybe (ChainOperator p))
        [Modifier p]
        (ExponentKind p)
        (XExponent p)

data ExponentKind p
    = Infinite (XInfinite p)
    | Slippery (XSlippery p)
    | Repeat Number (XRepeat p)

data Label p
    = Upper Char (XUpper p)
    | Descriptor String (XDescriptor p)
    | Leaper Number Number (XLeaper p)

type Number = Int

-- Summarizable instances: used for dot printing

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

-- Eq and Show
type EqX p =
    ( Eq (XOverride p)
    , Eq (XPlain p)
    , Eq (XUsing p)
    , Eq (XExport p)
    , Eq (XBare p)
    , Eq (XAssign p)
    , Eq (XAnonymous p)
    , Eq (XBetzaExpr p)
    , Eq (XChainExpr p)
    , Eq (XChainLeg p)
    , Eq (XUnionExpr p)
    , Eq (XModifierExpr p)
    , Eq (XExponentExpr p)
    , Eq (XParen p)
    , Eq (XFrom p)
    , Eq (XDirectional p)
    , Eq (XBehavioural p)
    , Eq (XAmalgamated p)
    , Eq (XSingle p)
    , Eq (XForward p)
    , Eq (XBackward p)
    , Eq (XLeftward p)
    , Eq (XRightward p)
    , Eq (XSideway p)
    , Eq (XVertically p)
    , Eq (XAll p)
    , Eq (XBehaviour p)
    , Eq (XCapture p)
    , Eq (XLeap p)
    , Eq (XInitial p)
    , Eq (XJump p)
    , Eq (XMove p)
    , Eq (XNoJump p)
    , Eq (XHop p)
    , Eq (XOnce p)
    , Eq (XTwice p)
    , Eq (XAny p)
    , Eq (XChainOperator p)
    , Eq (XStep p)
    , Eq (XSequence p)
    , Eq (XMandatory p)
    , Eq (XIffUnblocked p)
    , Eq (XChoose p)
    , Eq (XExponent p)
    , Eq (XInfinite p)
    , Eq (XSlippery p)
    , Eq (XRepeat p)
    , Eq (XUpper p)
    , Eq (XDescriptor p)
    , Eq (XLeaper p)
    )

deriving instance (EqX p) => Eq (QualifiedStmt p)
deriving instance (EqX p) => Eq (Directive p)
deriving instance (EqX p) => Eq (BetzaStmt p)
deriving instance (EqX p) => Eq (BetzaExpr p)
deriving instance (EqX p) => Eq (ChainExpr p)
deriving instance (EqX p) => Eq (ChainLeg p)
deriving instance (EqX p) => Eq (UnionExpr p)
deriving instance (EqX p) => Eq (ModifierExpr p)
deriving instance (EqX p) => Eq (ExponentExpr p)
deriving instance (EqX p) => Eq (AtomExpr p)
deriving instance (EqX p) => Eq (ChainOperator p)
deriving instance (EqX p) => Eq (ChainKind p)
deriving instance (EqX p) => Eq (ChainModality p)
deriving instance (EqX p) => Eq (Modifier p)
deriving instance (EqX p) => Eq (DirectionModifier p)
deriving instance (EqX p) => Eq (Direction p)
deriving instance (EqX p) => Eq (Behaviour p)
deriving instance (EqX p) => Eq (BehaviourKind p)
deriving instance (EqX p) => Eq (BehaviourModality p)
deriving instance (EqX p) => Eq (Exponent p)
deriving instance (EqX p) => Eq (ExponentKind p)
deriving instance (EqX p) => Eq (Label p)

type ShowX p =
    ( Show (XOverride p)
    , Show (XPlain p)
    , Show (XUsing p)
    , Show (XExport p)
    , Show (XBare p)
    , Show (XAssign p)
    , Show (XAnonymous p)
    , Show (XBetzaExpr p)
    , Show (XChainExpr p)
    , Show (XChainLeg p)
    , Show (XUnionExpr p)
    , Show (XModifierExpr p)
    , Show (XExponentExpr p)
    , Show (XParen p)
    , Show (XFrom p)
    , Show (XDirectional p)
    , Show (XBehavioural p)
    , Show (XAmalgamated p)
    , Show (XSingle p)
    , Show (XForward p)
    , Show (XBackward p)
    , Show (XLeftward p)
    , Show (XRightward p)
    , Show (XSideway p)
    , Show (XVertically p)
    , Show (XAll p)
    , Show (XBehaviour p)
    , Show (XCapture p)
    , Show (XLeap p)
    , Show (XInitial p)
    , Show (XJump p)
    , Show (XMove p)
    , Show (XNoJump p)
    , Show (XHop p)
    , Show (XOnce p)
    , Show (XTwice p)
    , Show (XAny p)
    , Show (XChainOperator p)
    , Show (XStep p)
    , Show (XSequence p)
    , Show (XMandatory p)
    , Show (XIffUnblocked p)
    , Show (XChoose p)
    , Show (XExponent p)
    , Show (XInfinite p)
    , Show (XSlippery p)
    , Show (XRepeat p)
    , Show (XUpper p)
    , Show (XDescriptor p)
    , Show (XLeaper p)
    )

deriving instance (ShowX p) => Show (QualifiedStmt p)
deriving instance (ShowX p) => Show (Directive p)
deriving instance (ShowX p) => Show (BetzaStmt p)
deriving instance (ShowX p) => Show (BetzaExpr p)
deriving instance (ShowX p) => Show (ChainExpr p)
deriving instance (ShowX p) => Show (ChainLeg p)
deriving instance (ShowX p) => Show (UnionExpr p)
deriving instance (ShowX p) => Show (ModifierExpr p)
deriving instance (ShowX p) => Show (ExponentExpr p)
deriving instance (ShowX p) => Show (AtomExpr p)
deriving instance (ShowX p) => Show (ChainOperator p)
deriving instance (ShowX p) => Show (ChainKind p)
deriving instance (ShowX p) => Show (ChainModality p)
deriving instance (ShowX p) => Show (Modifier p)
deriving instance (ShowX p) => Show (DirectionModifier p)
deriving instance (ShowX p) => Show (Direction p)
deriving instance (ShowX p) => Show (Behaviour p)
deriving instance (ShowX p) => Show (BehaviourKind p)
deriving instance (ShowX p) => Show (BehaviourModality p)
deriving instance (ShowX p) => Show (Exponent p)
deriving instance (ShowX p) => Show (ExponentKind p)
deriving instance (ShowX p) => Show (Label p)