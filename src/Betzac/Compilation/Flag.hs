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
        silentWs = SuppressWarnings `elem` flags
        specifierLevel s
            | warningsAreErrors || PromoteWarningsToError s `elem` flags = Error
            | not silentWs && GenerateWarnings s `elem` flags = Warning
            | otherwise = Silent
     in CompilerOptions
            { silentWarnings = silentWs
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
specifierOf Sem.DuplicateLabel = Just Wunused
specifierOf Sem.DuplicateDirective = Just Wdirective
specifierOf _ = Nothing

{- | Drop the diagnostics whose specifier is silenced, and force the rest to their
configured severity.
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
