{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Betzac.AST.Phases.Ds (
    Ds,
    DsX (..),
) where

import Betzac.AST.Generic
import Betzac.AST.Origin
import Betzac.AST.Types
import Betzac.Span

import Data.Data (Data)
import qualified Data.List.NonEmpty as NE
import Data.Void (Void)
import GHC.Generics

data Ds deriving (Data) -- desugared

data DsX = DsX {dsSpan :: Span, dsOrigin :: Origin}
    deriving (Eq, Show, Data, Generic)

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
type instance XSlippery Ds = Void -- X0* becomes X-{0}
type instance XRepeat Ds = DsX
type instance XUpper Ds = DsX
type instance XDescriptor Ds = DsX
type instance XLeaper Ds = DsX
type instance XQualification Ds = Qualification Ds
type instance XJoint Ds = ChainOperator Ds

instance HasOrigin DsX where
    origin = dsOrigin

instance Walk DsX where
    walk collector = gwalk collector . from
    {-# INLINE walk #-}

instance HasSpan DsX where
    getSpan = dsSpan

instance Qualifying Ds where
    toModifiers (Qualification ds bs) =
        map directional (NE.toList ds) <> map behavioural bs
    toJoint = Just

-- The wrapper carries nothing of its own, so it borrows what it wraps.
directional :: DirectionModifier Ds -> Modifier Ds
directional d = Directional d (DsX (getSpan d) (origin d))

behavioural :: Behaviour Ds -> Modifier Ds
behavioural b = Behavioural b (DsX (getSpan b) (origin b))