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
    let warningsAreErrors = PromoteAllWarningsToErrors `elem` flags
        specifierLevel s
            | warningsAreErrors || PromoteWarningsToError s `elem` flags = Error
            | GenerateWarnings s `elem` flags = Warning
            | otherwise = Silent
     in CompilerOptions
            { silentWarnings = SuppressWarnings `elem` flags
            , unusedLevel = specifierLevel Wunused
            , directiveLevel = specifierLevel Wdirective
            , langLevel = specifierLevel Wlang
            , terminateOnFirstError = TerminateOnFirstError `elem` flags
            }

levelFor :: CompilerOptions -> Wspecifier -> WarningLevel
levelFor opts Wunused = unusedLevel opts
levelFor opts Wdirective = directiveLevel opts
levelFor opts Wlang = langLevel opts

-- | The warning flag governing a problem kind. Kinds with none are always reported.
specifierOf :: Sem.SemanticProblemKind -> Maybe Wspecifier
specifierOf (Sem.UnusedLabel _) = Just Wunused
specifierOf Sem.UnusedUsing = Just Wunused
specifierOf Sem.DuplicateLabel = Just Wunused
specifierOf Sem.DuplicateDirective = Just Wdirective
specifierOf (Sem.RedundantModifier _) = Just Wlang
specifierOf Sem.RedundantOverride = Just Wdirective
specifierOf _ = Nothing

{- | Drop the diagnostics whose specifier is silenced, force the rest to their
configured severity, then drop whatever is still a warning if -w is set.
-}
applyOptions :: CompilerOptions -> [Sem.SemanticProblem] -> [Sem.SemanticProblem]
applyOptions opts = filter kept . mapMaybe adjust
  where
    adjust problem = case specifierOf (Sem.semKind problem) of
        Nothing -> Just problem
        Just spec -> case levelFor opts spec of
            Silent -> Nothing
            Warning -> Just problem{Sem.semSev = Sem.Warning}
            Error -> Just problem{Sem.semSev = Sem.Error}

    -- -w silences every warning, including the ones no -W flag governs. Anything
    -- promoted to an error on the way through outranks it.
    kept problem = not (silentWarnings opts) || Sem.semSev problem /= Sem.Warning
