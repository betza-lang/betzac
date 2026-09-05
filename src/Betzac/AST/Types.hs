{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Betzac.AST.Types (
    module Betzac.AST.Types.Ext,
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
    Qualifying (..),
) where

import Betzac.Span (HasSpan (..))

import Betzac.AST.Generic (GWalk (gwalk), Walk (..))
import Betzac.AST.Origin (HasOrigin (..))
import Betzac.AST.Types.Constraints
import Betzac.AST.Types.Ext

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics (Generic, from)

{- | Reading a modifier string back as the flat list the written form has, so that
rendering and stripping are polymorphic in the phase.
-}
class Qualifying p where
    toModifiers :: XQualification p -> [Modifier p]
    toJoint :: XJoint p -> Maybe (ChainOperator p)

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
    | Slippery !(XSlippery p)
    | Repeat Number (XRepeat p)

data Label p
    = Upper Char (XUpper p)
    | Descriptor Labelling (XDescriptor p)
    | Leaper Number Number (XLeaper p)

type Number = Int

type Labelling = String

-- Eq, Show, and Data
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

-- HasOrigin
instance (OriginX p) => HasOrigin (DirectionModifier p) where
    origin (Amalgamated _ _ ext) = origin ext
    origin (Single _ ext) = origin ext

instance (OriginX p) => HasOrigin (Direction p) where
    origin (Forward ext) = origin ext
    origin (Backward ext) = origin ext
    origin (Leftward ext) = origin ext
    origin (Rightward ext) = origin ext
    origin (Sideway ext) = origin ext
    origin (Vertically ext) = origin ext
    origin (All ext) = origin ext

instance (OriginX p) => HasOrigin (Behaviour p) where
    origin (Behaviour _ _ ext) = origin ext

instance (OriginX p) => HasOrigin (BehaviourKind p) where
    origin (Capture ext) = origin ext
    origin (Leap ext) = origin ext
    origin (Initial ext) = origin ext
    origin (Jump ext) = origin ext
    origin (Move ext) = origin ext
    origin (NoJump ext) = origin ext
    origin (Hop ext) = origin ext

instance (OriginX p) => HasOrigin (BehaviourModality p) where
    origin (Once ext) = origin ext
    origin (Twice ext) = origin ext
    origin (Any ext) = origin ext

instance (OriginX p) => HasOrigin (ChainOperator p) where
    origin (ChainOperator _ _ ext) = origin ext

instance (OriginX p) => HasOrigin (ChainKind p) where
    origin (Step ext) = origin ext
    origin (Sequence ext) = origin ext

instance (OriginX p) => HasOrigin (ChainModality p) where
    origin (Mandatory ext) = origin ext
    origin (Choose ext) = origin ext
    origin (IffUnblocked ext) = origin ext
