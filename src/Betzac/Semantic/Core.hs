{-# LANGUAGE InstanceSigs #-}

module Betzac.Semantic.Core (
    Pass,
    SemanticProblem (..),
    Severity (..),
    SemanticProblemKind (..),
    causeOf,
    mkProblem,
    emitErrorAt,
    emitWarningAt,
    runPass,
) where

import Betzac.Span (HasSpan (..), Span)
import Control.Monad.Trans.Writer (Writer, execWriter, tell)

import Betzac.Debug.PrettyPrint (PrettyPrint (..))

type Pass a = Writer [SemanticProblem] a

data Severity = Info | Warning | Error
    deriving (Show, Eq, Ord)

data SemanticProblemKind
    = InvalidValue String
    | AllInFirstLeg
    | IllFormedStatement
    | IllFormedLabel
    | UnresolvedLabel
    | UnusedLabel String
    | UnusedExpression
    | IllFormedDirective
    | UsingUnknown FilePath
    | UsingCircular [FilePath]
    | DuplicateDirective
    | DuplicateLabel
    | CompilationSucceeded
    | SystemFailure String
    | Unknown
    deriving (Show)

-- | The literal `cause` string for a given problem kind, as it should be rendered in a log.
causeOf :: SemanticProblemKind -> String
causeOf (InvalidValue _) = "invalid value"
causeOf AllInFirstLeg = "invalid value"
causeOf IllFormedStatement = "ill-formed statement"
causeOf IllFormedLabel = "ill-formed label"
causeOf UnresolvedLabel = "unresolved label"
causeOf (UnusedLabel _) = "unused label"
causeOf UnusedExpression = "unused expression"
causeOf IllFormedDirective = "ill-formed directive"
causeOf (UsingUnknown _) = "using unknown"
causeOf (UsingCircular _) = "using circular"
causeOf DuplicateDirective = "duplicate directive"
causeOf DuplicateLabel = "duplicate label"
causeOf CompilationSucceeded = "success"
causeOf (SystemFailure _) = "system"
causeOf Unknown = "unknown"

data SemanticProblem = SemanticProblem
    { semSev :: Severity
    , semKind :: SemanticProblemKind
    , semSpan :: Span
    }
    deriving (Show)

instance HasSpan SemanticProblem where
    getSpan = semSpan

-- | Build a 'SemanticProblem' directly, for call sites (e.g. multi-file discovery)
-- that aren't running inside a per-node 'Pass' traversal.
mkProblem :: Severity -> SemanticProblemKind -> Span -> SemanticProblem
mkProblem = SemanticProblem

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
