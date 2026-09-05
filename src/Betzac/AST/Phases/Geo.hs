{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Betzac.AST.Phases.Geo (
    Geo,
    GeoX (..),
) where

import Betzac.AST.Types
import Betzac.Span

import Data.Data (Data)
import GHC.Generics (Generic)

data Geo deriving (Data)

data GeoX = GeoX {geoSpan :: Span}
    deriving (Eq, Show, Data, Generic)

type instance XOverride Geo = GeoX
type instance XPlain Geo = GeoX
type instance XUsing Geo = GeoX
type instance XExport Geo = GeoX
type instance XBare Geo = GeoX
type instance XAssign Geo = GeoX
type instance XLabelRef Geo = GeoX
type instance XBetzaExpr Geo = GeoX
type instance XChainExpr Geo = GeoX
type instance XChainLeg Geo = GeoX
type instance XUnionExpr Geo = GeoX
type instance XModifierExpr Geo = GeoX
type instance XExponentExpr Geo = GeoX
type instance XParen Geo = GeoX
type instance XFrom Geo = GeoX
type instance XChainOperator Geo = GeoX
type instance XIffUnblocked Geo = GeoX
type instance XDirectional Geo = GeoX
type instance XBehavioural Geo = GeoX
type instance XAmalgamated Geo = GeoX
type instance XSingle Geo = GeoX
type instance XBehaviour Geo = GeoX
type instance XExponent Geo = GeoX
type instance XStep Geo = GeoX
type instance XSequence Geo = GeoX
type instance XMandatory Geo = ()
type instance XChoose Geo = GeoX
type instance XForward Geo = GeoX
type instance XBackward Geo = GeoX
type instance XLeftward Geo = GeoX
type instance XRightward Geo = GeoX
type instance XSideway Geo = GeoX
type instance XVertically Geo = GeoX
type instance XAll Geo = GeoX
type instance XCapture Geo = GeoX
type instance XLeap Geo = GeoX
type instance XInitial Geo = GeoX
type instance XJump Geo = GeoX
type instance XMove Geo = GeoX
type instance XNoJump Geo = GeoX
type instance XHop Geo = GeoX
type instance XOnce Geo = ()
type instance XTwice Geo = GeoX
type instance XAny Geo = GeoX
type instance XInfinite Geo = GeoX
type instance XSlippery Geo = GeoX
type instance XRepeat Geo = GeoX
type instance XUpper Geo = GeoX
type instance XDescriptor Geo = GeoX
type instance XLeaper Geo = GeoX
type instance XQualification Geo = Qualification Geo
type instance XJoint Geo = ChainOperator Geo

instance HasSpan GeoX where
    getSpan = geoSpan
