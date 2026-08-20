{-# LANGUAGE InstanceSigs #-}

module Betzac.Diagnostic (
    Pass,
    Stage,
    SemanticProblem (..),
    Severity (..),
    SemanticProblemKind (..),
    causeOf,
    mkProblem,
    emitErrorAt,
    emitWarningAt,
    runPass,
    stage,
    logProblems,
    runStage,
    runStage_,
) where

import Betzac.Debug.PrettyPrint (PrettyPrint (..))
import Betzac.Span (HasSpan (..), Span)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer (Writer, execWriter, runWriter, tell)

{- | A single-file semantic pass: accumulates 'SemanticProblem's as it goes, never
halts early.
-}
type Pass = Writer [SemanticProblem]

{- | One step of a cross-file diagnostic-producing pipeline: 'MaybeT' over 'Pass', so
diagnostics from every step run so far always accumulate, and 'stage' can signal "halt
the rest of the chain".
-}
type Stage = MaybeT Pass

data Severity = Info | Warning | Error
    deriving (Show, Eq, Ord)

data SemanticProblemKind
    = InvalidValue String
    | AllInFirstLeg
    | IllFormedStatement
    | IllFormedLabel
    | UnresolvedLabel
    | UnusedLabel String
    | UnusedUsing
    | IllFormedDirective
    | UsingUnknown FilePath
    | UsingCircular [FilePath]
    | CircularLabel [String]
    | DuplicateDirective
    | DuplicateLabel
    | UnnecessaryOverride
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
causeOf UnusedUsing = "unused using"
causeOf IllFormedDirective = "ill-formed directive"
causeOf (UsingUnknown _) = "using unknown"
causeOf (UsingCircular _) = "using circular"
causeOf (CircularLabel _) = "circular label"
causeOf DuplicateDirective = "duplicate directive"
causeOf DuplicateLabel = "duplicate label"
causeOf UnnecessaryOverride = "unnecessary override"
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

{- | Build a 'SemanticProblem' directly, for call sites (e.g. multi-file discovery)
that aren't running inside a per-node 'Pass' traversal.
-}
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

{- | Log 'ps'; halt the rest of the 'Stage' chain (later steps never get forced) if any
of 'ps' is Error-level, otherwise continue with 'a'.
-}
stage :: a -> [SemanticProblem] -> Stage a
stage a ps
    | any ((== Error) . semSev) ps = MaybeT (tell ps >> return Nothing)
    | otherwise = MaybeT (tell ps >> return (Just a))

{- | Log 'ps' without ever halting the chain — for combining independent diagnostic
sources uniformly with genuinely-gated 'stage' steps.
-}
logProblems :: [SemanticProblem] -> Stage ()
logProblems = lift . tell

runStage :: Stage a -> (Maybe a, [SemanticProblem])
runStage = runWriter . runMaybeT

runStage_ :: Stage a -> [SemanticProblem]
runStage_ = snd . runStage

instance PrettyPrint SemanticProblemKind where
    prettyPrint (InvalidValue s) = "Invalid Value: " <> s
    prettyPrint AllInFirstLeg = "`a` present in first leg of a chain"
    prettyPrint x = show x
