module Betzac.Compilation.Flag (
    Wspecifier (..),
    CompilerFlag (..),
    WarningLevel (..),
    CompilerOptions (..),
    optionsFromFlags,
    specifierOf,
    applyOptions,
) where

import Data.Maybe (mapMaybe)

import qualified Betzac.Diagnostic as Sem

data Wspecifier
    = Wunused
    | Wdirective
    | Wlang
    deriving (Eq)

data CompilerFlag
    = SuppressWarnings
    | GenerateWarnings Wspecifier
    | PromoteAllWarningsToErrors
    | PromoteWarningsToError Wspecifier
    | TerminateOnFirstError
    deriving (Eq)

data WarningLevel = Silent | Warning | Error
    deriving (Eq, Show)

data CompilerOptions = CompilerOptions
    { silentWarnings :: Bool
    , unusedLevel :: WarningLevel
    , directiveLevel :: WarningLevel
    , langLevel :: WarningLevel
    , terminateOnFirstError :: Bool
    }
    deriving (Show)

optionsFromFlags :: [CompilerFlag] -> CompilerOptions
optionsFromFlags flags =
    let warningsAreErrors = any (== PromoteAllWarningsToErrors) flags
        silentWs = any (== SuppressWarnings) flags
        specifierLevel s
            | warningsAreErrors || any (== PromoteWarningsToError s) flags = Error
            | not silentWs && any (== GenerateWarnings s) flags = Warning
            | otherwise = Silent
     in CompilerOptions
            { silentWarnings = silentWs
            , unusedLevel = specifierLevel Wunused
            , directiveLevel = specifierLevel Wdirective
            , langLevel = specifierLevel Wlang
            , terminateOnFirstError = any (== TerminateOnFirstError) flags
            }

levelFor :: CompilerOptions -> Wspecifier -> WarningLevel
levelFor opts Wunused = unusedLevel opts
levelFor opts Wdirective = directiveLevel opts
levelFor opts Wlang = langLevel opts

{- | The warning-flag specifier that governs a problem kind, if any. Kinds with no
governing specifier are unconditional (always reported, regardless of any flag) —
this covers hard correctness errors (ill-formed syntax, unresolved/circular/unknown
references, system failures) and job-level info logs.
-}
specifierOf :: Sem.SemanticProblemKind -> Maybe Wspecifier
specifierOf (Sem.UnusedLabel _) = Just Wunused
specifierOf Sem.UnusedExpression = Just Wunused
specifierOf Sem.DuplicateLabel = Just Wunused
specifierOf Sem.DuplicateDirective = Just Wdirective
specifierOf _ = Nothing

{- | Apply the compiler's warning-flag configuration to a batch of diagnostics: drop
any whose specifier is configured 'Silent', and force the severity of the rest to
match their specifier's configured level. Diagnostics with no governing specifier
pass through unchanged.
-}
applyOptions :: CompilerOptions -> [Sem.SemanticProblem] -> [Sem.SemanticProblem]
applyOptions opts = mapMaybe adjust
  where
    adjust problem = case specifierOf (Sem.semKind problem) of
        Nothing -> Just problem
        Just spec -> case levelFor opts spec of
            Silent -> Nothing
            Warning -> Just problem{Sem.semSev = Sem.Warning}
            Error -> Just problem{Sem.semSev = Sem.Error}