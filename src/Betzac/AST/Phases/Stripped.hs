{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Betzac.AST.Phases.Stripped (Stripped) where

import Betzac.AST.Types

import Data.Data (Data)

data Stripped deriving (Data)

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

instance Qualifying Stripped where
    toModifiers = id
    toJoint = id
