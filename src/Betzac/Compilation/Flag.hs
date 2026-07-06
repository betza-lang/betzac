module Betzac.Compilation.Flag (
    Wspecifier (..),
    CompilerFlag (..),
    WarningLevel (..),
    CompilerOptions (..),
    optionsFromFlags,
) where

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