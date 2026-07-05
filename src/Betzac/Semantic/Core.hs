{-# LANGUAGE InstanceSigs #-}

module Betzac.Semantic.Core (
    Pass,
    SemanticProblem (..),
    Severity (..),
    SemanticProblemKind (..),
    emitErrorAt,
    emitWarningAt,
    runPass,
) where

import Betzac.Span (HasSpan (..), Span)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)

import Betzac.Debug.PrettyPrint (PrettyPrint (..))

type Pass a = Writer [SemanticProblem] a

data Severity = Warning | Error
    deriving (Show, Eq, Ord)

data SemanticProblemKind = InvalidValue String | AllInFirstLeg | Unknown
    deriving (Show)

data SemanticProblem = SemanticProblem
    { semSev :: Severity
    , semKind :: SemanticProblemKind
    , semSpan :: Span
    }
    deriving (Show)

instance HasSpan SemanticProblem where
    getSpan = semSpan

emitAt :: (HasSpan x) => Severity -> SemanticProblemKind -> x -> Pass ()
emitAt s k node = tell [(SemanticProblem s k (getSpan node))]

emitErrorAt :: (HasSpan x) => SemanticProblemKind -> x -> Pass ()
emitErrorAt = emitAt Error

emitWarningAt :: (HasSpan x) => SemanticProblemKind -> x -> Pass ()
emitWarningAt = emitAt Warning

runPass :: Pass () -> [SemanticProblem]
runPass = execWriter

-- pretty print problems

instance PrettyPrint SemanticProblemKind where
    prettyPrint (InvalidValue s) = "Invalid Value: " <> s
    prettyPrint AllInFirstLeg = "`a` present in first leg of a chain"
    prettyPrint x = show x
