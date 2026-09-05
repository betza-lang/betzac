{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Betzac.AST.Phases.Ps (
    Ps,
    PsX (..),
) where

import Betzac.AST.Types
import Betzac.Span

import Betzac.AST.Generic
import Betzac.AST.Origin
import Data.Data (Data)
import GHC.Generics (Generic, from)

data Ps deriving (Data) -- parsed

data PsX = PsX {psSpan :: Span}
    deriving (Eq, Show, Data, Generic)

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

instance HasOrigin PsX where -- nothing is implied before Ds
    origin _ = Written

instance Walk PsX where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance HasSpan PsX where
    getSpan = psSpan

instance Qualifying Ps where
    toModifiers = id
    toJoint = id
