{-# LANGUAGE TypeFamilies #-}

module Betzac.AST.Types.Ext (
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
) where

import Data.Kind (Type)

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

-- Field-position families: the phase decides the data, not just an annotation.
type family XQualification (p :: Type) :: Type
type family XJoint (p :: Type) :: Type
