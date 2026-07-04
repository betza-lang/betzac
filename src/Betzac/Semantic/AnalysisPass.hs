module Betzac.Semantic.AnalysisPass (
    Pass,
    PassResult (..),
    SemanticError (..),
    Severity (..),
    SemanticErrorKind (..),
    emitErrorAt,
    emitWarningAt,
    runPass,
    fromWriter,
) where

import Betzac.Located (Located (..))
import Control.Monad.Trans.Writer (Writer, execWriter, tell)

type Pass a = Writer [Located SemanticError] a

data PassResult a = PassResult
    { passOutput :: a
    , passErrors :: [Located SemanticError]
    , passWarnings :: [Located SemanticError]
    }

data Severity = Error | Warning
    deriving (Eq)

data SemanticErrorKind = InvalidValue String | AllInFirstLeg

data SemanticError = SemanticError
    { severity :: Severity
    , errorKind :: SemanticErrorKind
    }

emitAt :: Located a -> Severity -> SemanticErrorKind -> Pass ()
emitAt node s k = tell [Located (startPos node) (endPos node) (SemanticError s k)]

emitErrorAt :: Located a -> SemanticErrorKind -> Pass ()
emitErrorAt node = emitAt node Error

emitWarningAt :: Located a -> SemanticErrorKind -> Pass ()
emitWarningAt node = emitAt node Warning

runPass :: Pass () -> PassResult ()
runPass p = fromWriter () (execWriter p)

fromWriter :: a -> [Located SemanticError] -> PassResult a
fromWriter a diags = PassResult a errors warnings
  where
    errors = filter ((== Error) . severity . tokenVal) diags
    warnings = filter ((== Warning) . severity . tokenVal) diags
