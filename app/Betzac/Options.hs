module Options (
    Options,
    Verbosity (..),
    inputFile,
    emitDot,
    workspace,
    preludePath,
    verbosity,
    compilerFlags,
    getOptions,
) where

import Betzac.Compilation.Flag (CompilerFlag (..), Wspecifier (..))
import Betzac.Version (versionString)

import Data.List (stripPrefix)
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O (Parser)
import System.Exit (exitSuccess)

data Verbosity = Silent | Verbose | VeryVerbose deriving (Eq, Ord)

data Options = Options
    { inputFile :: FilePath
    , emitDot :: Maybe FilePath
    , workspace :: Maybe FilePath
    , preludePath :: Maybe FilePath
    , verbosity :: Verbosity
    , compilerFlags :: [CompilerFlag]
    }

{- | What the command line asked for. Reporting the version is its own mode rather
than a flag on a compilation, so it is accepted alone and nowhere else.
-}
data Invocation = ReportVersion | Compile Options

getOptions :: IO Options
getOptions = do
    invocation <-
        execParser $
            info
                (invocationParser <**> helper)
                ( fullDesc
                    <> progDesc "Compile a betza file"
                    <> header "betzac - the betza compiler"
                )
    case invocation of
        ReportVersion -> putStrLn (versionString "betzac") >> exitSuccess
        Compile opts -> return opts

invocationParser :: O.Parser Invocation
invocationParser = versionParser <|> (Compile <$> optionsParser)

versionParser :: O.Parser Invocation
versionParser =
    ReportVersion
        <$ flag'
            ()
            ( long "version"
                <> help "Show the version and exit (accepted on its own)"
            )

optionsParser :: O.Parser Options
optionsParser =
    Options
        <$> argument str (metavar "FILE" <> help "Input betza file")
        <*> optional
            ( strOption $
                long "dot"
                    <> metavar "FILE"
                    <> value "/dev/null"
                    <> showDefault
                    <> help "Write AST as dot graph to FILE, or - for stdout"
            )
        <*> optional
            ( strOption $
                long "workspace"
                    <> metavar "DIR"
                    <> help "Workspace directory `using` directives resolve against (defaults to the input file's directory)"
            )
        <*> optional
            ( strOption $
                long "prelude"
                    <> metavar "FILE"
                    <> help "Standard prelude to compile against (defaults to $BETZAC_PRELUDE, else the installed one)"
            )
        <*> verbosityParser
        <*> compilerFlagsParser

verbosityParser :: O.Parser Verbosity
verbosityParser = toVerbosity <$> many (flag' () (short 'v' <> long "verbose" <> help "Verbosity (-v or -vv)"))
  where
    toVerbosity [] = Silent
    toVerbosity [_] = Verbose
    toVerbosity _ = VeryVerbose

compilerFlagsParser :: O.Parser [CompilerFlag]
compilerFlagsParser =
    (\suppress wflags -> [SuppressWarnings | suppress] ++ concat wflags)
        <$> switch (short 'w' <> help "Suppress all warnings")
        <*> many
            ( option
                (eitherReader parseWFlag)
                (short 'W' <> metavar "FLAG" <> help "all|error|error=SPEC|fatal-errors|unused|directive|lang")
            )

parseWFlag :: String -> Either String [CompilerFlag]
parseWFlag "all" = Right [GenerateWarnings Wunused, GenerateWarnings Wdirective, GenerateWarnings Wlang]
parseWFlag "error" = Right [PromoteAllWarningsToErrors]
parseWFlag "fatal-errors" = Right [TerminateOnFirstError]
parseWFlag "unused" = Right [GenerateWarnings Wunused]
parseWFlag "directive" = Right [GenerateWarnings Wdirective]
parseWFlag "lang" = Right [GenerateWarnings Wlang]
parseWFlag s
    | Just rest <- stripPrefix "error=" s = case parseSpecifier rest of
        Just w -> Right [PromoteWarningsToError w]
        Nothing -> Left ("unknown warning specifier for -Werror=: " ++ rest)
    | otherwise = Left ("unknown flag: -W" ++ s)

parseSpecifier :: String -> Maybe Wspecifier
parseSpecifier "unused" = Just Wunused
parseSpecifier "directive" = Just Wdirective
parseSpecifier "lang" = Just Wlang
parseSpecifier _ = Nothing
