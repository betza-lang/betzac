{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Betzac.AST.Types (
    -- Extension typee families
    XOverride,
    XPlain,
    XUsing,
    XExport,
    XBare,
    XAssign,
    XLabelRef,
    XBetzaExpr,
    XChainExpr,
    XChainLeg,
    XUnionExpr,
    XModifierExpr,
    XExponentExpr,
    XParen,
    XFrom,
    XChainOperator,
    XIffUnblocked,
    XDirectional,
    XBehavioural,
    XAmalgamated,
    XSingle,
    XBehaviour,
    XExponent,
    XStep,
    XSequence,
    XMandatory,
    XChoose,
    XForward,
    XBackward,
    XLeftward,
    XRightward,
    XSideway,
    XVertically,
    XAll,
    XCapture,
    XLeap,
    XInitial,
    XJump,
    XMove,
    XNoJump,
    XHop,
    XOnce,
    XTwice,
    XAny,
    XInfinite,
    XSlippery,
    XRepeat,
    XUpper,
    XDescriptor,
    XLeaper,
    XQualification,
    XJoint,
    -- AST types
    BetzaProgram,
    QualifiedStmt (..),
    Directive (..),
    BetzaStmt (..),
    BetzaExpr (..),
    ChainExpr (..),
    ChainLeg (..),
    UnionExpr (..),
    ModifierExpr (..),
    ExponentExpr (..),
    AtomExpr (..),
    ChainOperator (..),
    ChainKind (..),
    ChainModality (..),
    Modifier (..),
    DirectionModifier (..),
    Direction (..),
    Behaviour (..),
    BehaviourKind (..),
    BehaviourModality (..),
    Qualification (..),
    Exponent (..),
    ExponentKind (..),
    Label (..),
    Number,
    Labelling,
    OriginX,
) where

import Betzac.Span (HasSpan (..))

import Betzac.AST.Generic (GWalk (gwalk), Walk (..), WalkField)
import Betzac.AST.Origin (HasOrigin (..))
import Data.Data (Data, Typeable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic, from)

-- Extension type families, giving these constructors annotations
-- XConstructorName Phase = FieldType
-- Statement/directive level
type family XOverride (p :: Type) :: Type
type family XPlain (p :: Type) :: Type
type family XUsing (p :: Type) :: Type
type family XExport (p :: Type) :: Type
type family XBare (p :: Type) :: Type
type family XAssign (p :: Type) :: Type
type family XLabelRef (p :: Type) :: Type

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

-- Field-position families: the phase decides the shape, not just an annotation.
type family XQualification (p :: Type) :: Type
type family XJoint (p :: Type) :: Type

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
    | LabelRef (Label p) (XLabelRef p)

data BetzaExpr p = BetzaExpr (ChainExpr p) (XBetzaExpr p)

data ChainExpr p = ChainExpr (UnionExpr p) (Maybe (ChainLeg p)) (XChainExpr p)

data ChainLeg p = ChainLeg (ChainOperator p) (ChainExpr p) (XChainLeg p)

data UnionExpr p = UnionExpr (NonEmpty (ModifierExpr p)) (XUnionExpr p)

data ModifierExpr p = ModifierExpr
    { setup :: Bool
    , modifiers :: XQualification p
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

-- | A modifier string once the phase guarantees what it must contain.
data Qualification p = Qualification
    { qualDirections :: NonEmpty (DirectionModifier p)
    , qualBehaviours :: [Behaviour p]
    }

data Exponent p
    = Exponent
        (XJoint p)
        (XQualification p)
        (ExponentKind p)
        (XExponent p)

data ExponentKind p
    = Infinite (XInfinite p)
    | Slippery (XSlippery p)
    | Repeat Number (XRepeat p)

data Label p
    = Upper Char (XUpper p)
    | Descriptor Labelling (XDescriptor p)
    | Leaper Number Number (XLeaper p)

type Number = Int

type Labelling = String

-- Eq, Show, and Data
type EqX p =
    ( Eq (XOverride p)
    , Eq (XPlain p)
    , Eq (XUsing p)
    , Eq (XExport p)
    , Eq (XBare p)
    , Eq (XAssign p)
    , Eq (XLabelRef p)
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
    , Eq (XQualification p)
    , Eq (XJoint p)
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
deriving instance (EqX p) => Eq (Qualification p)
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
    , Show (XLabelRef p)
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
    , Show (XQualification p)
    , Show (XJoint p)
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
deriving instance (ShowX p) => Show (Qualification p)
deriving instance (ShowX p) => Show (Exponent p)
deriving instance (ShowX p) => Show (ExponentKind p)
deriving instance (ShowX p) => Show (Label p)

type DataX p =
    ( Typeable p
    , Data p
    , Data (XOverride p)
    , Data (XPlain p)
    , Data (XUsing p)
    , Data (XExport p)
    , Data (XBare p)
    , Data (XAssign p)
    , Data (XLabelRef p)
    , Data (XBetzaExpr p)
    , Data (XChainExpr p)
    , Data (XChainLeg p)
    , Data (XUnionExpr p)
    , Data (XModifierExpr p)
    , Data (XExponentExpr p)
    , Data (XParen p)
    , Data (XFrom p)
    , Data (XDirectional p)
    , Data (XBehavioural p)
    , Data (XAmalgamated p)
    , Data (XSingle p)
    , Data (XForward p)
    , Data (XBackward p)
    , Data (XLeftward p)
    , Data (XRightward p)
    , Data (XSideway p)
    , Data (XVertically p)
    , Data (XAll p)
    , Data (XBehaviour p)
    , Data (XCapture p)
    , Data (XLeap p)
    , Data (XInitial p)
    , Data (XJump p)
    , Data (XMove p)
    , Data (XNoJump p)
    , Data (XHop p)
    , Data (XOnce p)
    , Data (XTwice p)
    , Data (XAny p)
    , Data (XChainOperator p)
    , Data (XStep p)
    , Data (XSequence p)
    , Data (XMandatory p)
    , Data (XIffUnblocked p)
    , Data (XChoose p)
    , Data (XExponent p)
    , Data (XInfinite p)
    , Data (XSlippery p)
    , Data (XRepeat p)
    , Data (XUpper p)
    , Data (XDescriptor p)
    , Data (XLeaper p)
    , Data (XQualification p)
    , Data (XJoint p)
    )

deriving instance (DataX p) => Data (QualifiedStmt p)
deriving instance (DataX p) => Data (Directive p)
deriving instance (DataX p) => Data (BetzaStmt p)
deriving instance (DataX p) => Data (BetzaExpr p)
deriving instance (DataX p) => Data (ChainExpr p)
deriving instance (DataX p) => Data (ChainLeg p)
deriving instance (DataX p) => Data (UnionExpr p)
deriving instance (DataX p) => Data (ModifierExpr p)
deriving instance (DataX p) => Data (ExponentExpr p)
deriving instance (DataX p) => Data (AtomExpr p)
deriving instance (DataX p) => Data (ChainOperator p)
deriving instance (DataX p) => Data (ChainKind p)
deriving instance (DataX p) => Data (ChainModality p)
deriving instance (DataX p) => Data (Modifier p)
deriving instance (DataX p) => Data (DirectionModifier p)
deriving instance (DataX p) => Data (Direction p)
deriving instance (DataX p) => Data (Behaviour p)
deriving instance (DataX p) => Data (BehaviourKind p)
deriving instance (DataX p) => Data (BehaviourModality p)
deriving instance (DataX p) => Data (Qualification p)
deriving instance (DataX p) => Data (Exponent p)
deriving instance (DataX p) => Data (ExponentKind p)
deriving instance (DataX p) => Data (Label p)

-- Generic traversal
type WalkX p =
    ( Typeable p
    , WalkField (XOverride p)
    , WalkField (XPlain p)
    , WalkField (XUsing p)
    , WalkField (XExport p)
    , WalkField (XBare p)
    , WalkField (XAssign p)
    , WalkField (XLabelRef p)
    , WalkField (XBetzaExpr p)
    , WalkField (XChainExpr p)
    , WalkField (XChainLeg p)
    , WalkField (XUnionExpr p)
    , WalkField (XModifierExpr p)
    , WalkField (XExponentExpr p)
    , WalkField (XParen p)
    , WalkField (XFrom p)
    , WalkField (XDirectional p)
    , WalkField (XBehavioural p)
    , WalkField (XAmalgamated p)
    , WalkField (XSingle p)
    , WalkField (XForward p)
    , WalkField (XBackward p)
    , WalkField (XLeftward p)
    , WalkField (XRightward p)
    , WalkField (XSideway p)
    , WalkField (XVertically p)
    , WalkField (XAll p)
    , WalkField (XBehaviour p)
    , WalkField (XCapture p)
    , WalkField (XLeap p)
    , WalkField (XInitial p)
    , WalkField (XJump p)
    , WalkField (XMove p)
    , WalkField (XNoJump p)
    , WalkField (XHop p)
    , WalkField (XOnce p)
    , WalkField (XTwice p)
    , WalkField (XAny p)
    , WalkField (XChainOperator p)
    , WalkField (XStep p)
    , WalkField (XSequence p)
    , WalkField (XMandatory p)
    , WalkField (XIffUnblocked p)
    , WalkField (XChoose p)
    , WalkField (XExponent p)
    , WalkField (XInfinite p)
    , WalkField (XSlippery p)
    , WalkField (XRepeat p)
    , WalkField (XUpper p)
    , WalkField (XDescriptor p)
    , WalkField (XLeaper p)
    , WalkField (XQualification p)
    , WalkField (XJoint p)
    )

deriving instance Generic (QualifiedStmt p)
deriving instance Generic (Directive p)
deriving instance Generic (BetzaStmt p)
deriving instance Generic (BetzaExpr p)
deriving instance Generic (ChainExpr p)
deriving instance Generic (ChainLeg p)
deriving instance Generic (UnionExpr p)
deriving instance Generic (ModifierExpr p)
deriving instance Generic (ExponentExpr p)
deriving instance Generic (AtomExpr p)
deriving instance Generic (ChainOperator p)
deriving instance Generic (ChainKind p)
deriving instance Generic (ChainModality p)
deriving instance Generic (Modifier p)
deriving instance Generic (DirectionModifier p)
deriving instance Generic (Direction p)
deriving instance Generic (Behaviour p)
deriving instance Generic (BehaviourKind p)
deriving instance Generic (BehaviourModality p)
deriving instance Generic (Qualification p)
deriving instance Generic (Exponent p)
deriving instance Generic (ExponentKind p)
deriving instance Generic (Label p)

instance (WalkX p) => Walk (QualifiedStmt p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Directive p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (BetzaStmt p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (BetzaExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ChainExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ChainLeg p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (UnionExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ModifierExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ExponentExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (AtomExpr p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ChainOperator p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ChainKind p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ChainModality p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Modifier p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (DirectionModifier p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Direction p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Behaviour p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (BehaviourKind p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (BehaviourModality p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Qualification p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Exponent p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (ExponentKind p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance (WalkX p) => Walk (Label p) where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

-- HasSpan
type SpanX p =
    ( HasSpan (XOverride p)
    , HasSpan (XPlain p)
    , HasSpan (XUsing p)
    , HasSpan (XExport p)
    , HasSpan (XBare p)
    , HasSpan (XAssign p)
    , HasSpan (XLabelRef p)
    , HasSpan (XBetzaExpr p)
    , HasSpan (XChainExpr p)
    , HasSpan (XChainLeg p)
    , HasSpan (XUnionExpr p)
    , HasSpan (XModifierExpr p)
    , HasSpan (XExponentExpr p)
    , HasSpan (XParen p)
    , HasSpan (XFrom p)
    , HasSpan (XChainOperator p)
    , HasSpan (XStep p)
    , HasSpan (XSequence p)
    , HasSpan (XMandatory p)
    , HasSpan (XChoose p)
    , HasSpan (XIffUnblocked p)
    , HasSpan (XDirectional p)
    , HasSpan (XBehavioural p)
    , HasSpan (XAmalgamated p)
    , HasSpan (XSingle p)
    , HasSpan (XForward p)
    , HasSpan (XBackward p)
    , HasSpan (XLeftward p)
    , HasSpan (XRightward p)
    , HasSpan (XSideway p)
    , HasSpan (XVertically p)
    , HasSpan (XAll p)
    , HasSpan (XBehaviour p)
    , HasSpan (XCapture p)
    , HasSpan (XLeap p)
    , HasSpan (XInitial p)
    , HasSpan (XJump p)
    , HasSpan (XMove p)
    , HasSpan (XNoJump p)
    , HasSpan (XHop p)
    , HasSpan (XOnce p)
    , HasSpan (XTwice p)
    , HasSpan (XAny p)
    , HasSpan (XExponent p)
    , HasSpan (XInfinite p)
    , HasSpan (XSlippery p)
    , HasSpan (XRepeat p)
    , HasSpan (XUpper p)
    , HasSpan (XDescriptor p)
    , HasSpan (XLeaper p)
    )

instance (SpanX p) => HasSpan (QualifiedStmt p) where
    getSpan (Override _ ext) = getSpan ext
    getSpan (Plain _ ext) = getSpan ext

instance (SpanX p) => HasSpan (Directive p) where
    getSpan (Using _ ext) = getSpan ext
    getSpan (Export _ ext) = getSpan ext
    getSpan (Bare _ ext) = getSpan ext

instance (SpanX p) => HasSpan (BetzaStmt p) where
    getSpan (Assign _ _ ext) = getSpan ext
    getSpan (LabelRef _ ext) = getSpan ext

instance (SpanX p) => HasSpan (BetzaExpr p) where
    getSpan (BetzaExpr _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ChainExpr p) where
    getSpan (ChainExpr _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ChainLeg p) where
    getSpan (ChainLeg _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (UnionExpr p) where
    getSpan (UnionExpr _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ModifierExpr p) where
    getSpan = getSpan . modExprExt

instance (SpanX p) => HasSpan (ExponentExpr p) where
    getSpan (ExponentExpr _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (AtomExpr p) where
    getSpan (Paren _ ext) = getSpan ext
    getSpan (From _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ChainOperator p) where
    getSpan (ChainOperator _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ChainKind p) where
    getSpan (Step ext) = getSpan ext
    getSpan (Sequence ext) = getSpan ext

instance (SpanX p) => HasSpan (ChainModality p) where
    getSpan (Mandatory ext) = getSpan ext
    getSpan (Choose ext) = getSpan ext
    getSpan (IffUnblocked ext) = getSpan ext

instance (SpanX p) => HasSpan (Modifier p) where
    getSpan (Directional _ ext) = getSpan ext
    getSpan (Behavioural _ ext) = getSpan ext

instance (SpanX p) => HasSpan (DirectionModifier p) where
    getSpan (Amalgamated _ _ ext) = getSpan ext
    getSpan (Single _ ext) = getSpan ext

instance (SpanX p) => HasSpan (Direction p) where
    getSpan (Forward ext) = getSpan ext
    getSpan (Backward ext) = getSpan ext
    getSpan (Leftward ext) = getSpan ext
    getSpan (Rightward ext) = getSpan ext
    getSpan (Sideway ext) = getSpan ext
    getSpan (Vertically ext) = getSpan ext
    getSpan (All ext) = getSpan ext

instance (SpanX p) => HasSpan (Behaviour p) where
    getSpan (Behaviour _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (BehaviourKind p) where
    getSpan (Capture ext) = getSpan ext
    getSpan (Leap ext) = getSpan ext
    getSpan (Initial ext) = getSpan ext
    getSpan (Jump ext) = getSpan ext
    getSpan (Move ext) = getSpan ext
    getSpan (NoJump ext) = getSpan ext
    getSpan (Hop ext) = getSpan ext

instance (SpanX p) => HasSpan (BehaviourModality p) where
    getSpan (Once ext) = getSpan ext
    getSpan (Twice ext) = getSpan ext
    getSpan (Any ext) = getSpan ext

instance (SpanX p) => HasSpan (Exponent p) where
    getSpan (Exponent _ _ _ ext) = getSpan ext

instance (SpanX p) => HasSpan (ExponentKind p) where
    getSpan (Infinite ext) = getSpan ext
    getSpan (Slippery ext) = getSpan ext
    getSpan (Repeat _ ext) = getSpan ext

instance (SpanX p) => HasSpan (Label p) where
    getSpan (Upper _ ext) = getSpan ext
    getSpan (Descriptor _ ext) = getSpan ext
    getSpan (Leaper _ _ ext) = getSpan ext

-- HasOrigin, for the nodes a desugaring can supply or find already written
type OriginX p =
    ( HasOrigin (XAmalgamated p)
    , HasOrigin (XSingle p)
    , HasOrigin (XBehaviour p)
    )

instance (OriginX p) => HasOrigin (DirectionModifier p) where
    origin (Amalgamated _ _ ext) = origin ext
    origin (Single _ ext) = origin ext

instance (OriginX p) => HasOrigin (Behaviour p) where
    origin (Behaviour _ _ ext) = origin ext
