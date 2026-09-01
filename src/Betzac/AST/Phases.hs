{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Betzac.AST.Phases (
    Stripped,
    Ps,
    PsX (..),
    Ds,
    DsX (..),
    Qualifying (..),
) where

import Betzac.AST.Generic (GWalk (gwalk), Walk (..))
import Betzac.AST.Origin (HasOrigin (..), Origin (Written))
import Betzac.AST.Types
import Betzac.Span (HasSpan (..), Span (Generated))
import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic, from)

-- Phase indices
data Stripped deriving (Data) -- all extensions are unit type
data Ps deriving (Data) -- parsed
data Ds deriving (Data) -- desugared

-- Global extension records
-- for Ps
data PsX = PsX {psSpan :: Span}
    deriving (Eq, Show, Data, Generic)

-- for Ds
data DsX = DsX {dsSpan :: Span, dsOrigin :: Origin}
    deriving (Eq, Show, Data, Generic)

instance HasOrigin PsX where -- nothing is implied before Ds
    origin _ = Written

instance HasOrigin DsX where
    origin = dsOrigin

instance Walk PsX where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance Walk DsX where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

-- Stripped phase instances
type instance XOverride Stripped = ()
type instance XPlain Stripped = ()
type instance XUsing Stripped = ()
type instance XExport Stripped = ()
type instance XBare Stripped = ()
type instance XAssign Stripped = ()
type instance XLabelRef Stripped = ()
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
type instance XQualification Stripped = [Modifier Stripped]
type instance XJoint Stripped = Maybe (ChainOperator Stripped)

-- Ps phase instances
type instance XOverride Ps = PsX
type instance XPlain Ps = PsX
type instance XUsing Ps = PsX
type instance XExport Ps = PsX
type instance XBare Ps = PsX
type instance XAssign Ps = PsX
type instance XLabelRef Ps = PsX
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
type instance XQualification Ps = [Modifier Ps]
type instance XJoint Ps = Maybe (ChainOperator Ps)

-- Ds phase instances
type instance XOverride Ds = DsX
type instance XPlain Ds = DsX
type instance XUsing Ds = DsX
type instance XExport Ds = DsX
type instance XBare Ds = DsX
type instance XAssign Ds = DsX
type instance XLabelRef Ds = DsX
type instance XBetzaExpr Ds = DsX
type instance XChainExpr Ds = DsX
type instance XChainLeg Ds = DsX
type instance XUnionExpr Ds = DsX
type instance XModifierExpr Ds = DsX
type instance XExponentExpr Ds = DsX
type instance XParen Ds = DsX
type instance XFrom Ds = DsX
type instance XChainOperator Ds = DsX
type instance XIffUnblocked Ds = DsX
type instance XDirectional Ds = DsX
type instance XBehavioural Ds = DsX
type instance XAmalgamated Ds = DsX
type instance XSingle Ds = DsX
type instance XBehaviour Ds = DsX
type instance XExponent Ds = DsX
type instance XStep Ds = DsX
type instance XSequence Ds = DsX
type instance XMandatory Ds = ()
type instance XChoose Ds = DsX
type instance XForward Ds = DsX
type instance XBackward Ds = DsX
type instance XLeftward Ds = DsX
type instance XRightward Ds = DsX
type instance XSideway Ds = DsX
type instance XVertically Ds = DsX
type instance XAll Ds = DsX
type instance XCapture Ds = DsX
type instance XLeap Ds = DsX
type instance XInitial Ds = DsX
type instance XJump Ds = DsX
type instance XMove Ds = DsX
type instance XNoJump Ds = DsX
type instance XHop Ds = DsX
type instance XOnce Ds = ()
type instance XTwice Ds = DsX
type instance XAny Ds = DsX
type instance XInfinite Ds = DsX
type instance XSlippery Ds = DsX
type instance XRepeat Ds = DsX
type instance XUpper Ds = DsX
type instance XDescriptor Ds = DsX
type instance XLeaper Ds = DsX
type instance XQualification Ds = Qualification Ds
type instance XJoint Ds = ChainOperator Ds

-- HasSpan

instance HasSpan () where -- Stripped phase, for uniformity
    getSpan () = Generated

instance HasSpan PsX where
    getSpan = psSpan

instance HasSpan DsX where
    getSpan = dsSpan

{- | Reading a modifier string back as the flat list the written form has, so that
rendering and stripping stay polymorphic in the phase.
-}
class Qualifying p where
    toModifiers :: XQualification p -> [Modifier p]
    toJoint :: XJoint p -> Maybe (ChainOperator p)

instance Qualifying Stripped where
    toModifiers = id
    toJoint = id

instance Qualifying Ps where
    toModifiers = id
    toJoint = id

instance Qualifying Ds where
    toModifiers (Qualification ds bs) =
        map directional (NE.toList ds) <> map behavioural bs
    toJoint = Just

-- The wrapper carries nothing of its own, so it borrows what it wraps.
directional :: DirectionModifier Ds -> Modifier Ds
directional d = Directional d (DsX (getSpan d) (origin d))

behavioural :: Behaviour Ds -> Modifier Ds
behavioural b = Behavioural b (DsX (getSpan b) (origin b))
