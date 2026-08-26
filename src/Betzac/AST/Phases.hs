{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Betzac.AST.Phases (
    Stripped,
    Ps,
    PsX (..),
) where

import Betzac.AST.Generic (GWalk (gwalk), Walk (..))
import Betzac.AST.Types
import Betzac.Span (HasSpan (..), Span (Generated))
import Data.Data (Data)
import GHC.Generics (Generic, from)

-- Phase indices
data Stripped deriving (Data) -- all extensions are unit type
data Ps deriving (Data) -- parsed
-- data Ds -- desugared

-- Global extension records
-- for Ps
data PsX = PsX {psSpan :: Span}
    deriving (Eq, Show, Data, Generic)

instance Walk PsX where
    walk = gwalk . from
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

-- HasSpan

instance HasSpan () where -- Stripped phase, for uniformity
    getSpan () = Generated

instance HasSpan PsX where
    getSpan = psSpan
