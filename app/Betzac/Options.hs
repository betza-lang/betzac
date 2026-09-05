module Options (
    Options,
    Verbosity (..),
    AstPhase (..),
    DotRequest (..),
    inputFile,
    emitDot,
    workspace,
    preludePath,
    verbosity,
    compilerFlags,
    getOptions,
) where

import Betzac.Compilation.Flag (CompilerFlag (..), Wspecifier (..))
import Betzac.Utils.Version (versionString)

import Data.List (stripPrefix)
import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O (Parser)
import System.Exit (exitFailure, exitSuccess)
import qualified System.IO as S

data Verbosity = Silent | Verbose | VeryVerbose deriving (Eq, Ord)

-- | Which tree @--dot@ should draw. TODO: geo, once the phase exists.
data AstPhase = PhasePs | PhaseDs deriving (Eq)

instance Show AstPhase where
    show PhasePs = "ps"
    show PhaseDs = "ds"

-- | Where to write the graph, and which phase to draw; the latest produced if unsaid.
data DotRequest = DotRequest
    { dotPath :: FilePath
    , dotPhase :: Maybe AstPhase
    }

data Options = Options
    { inputFile :: FilePath
    , emitDot :: Maybe DotRequest
    , workspace :: Maybe FilePath
    , preludePath :: Maybe FilePath
    , verbosity :: Verbosity
    , compilerFlags :: [CompilerFlag]
    }

{- | What the command line asked for. Reporting the version is its own mode rather
than a flag on a compilation, so it is accepted alone and nowhere else.
-}
data Invocation = ReportVersion | Compile (Either String Options)

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
        Compile (Left problem) -> S.hPutStrLn S.stderr problem >> exitFailure
        Compile (Right opts) -> return opts

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

optionsParser :: O.Parser (Either String Options)
optionsParser =
    assemble
        <$> argument str (metavar "FILE" <> help "Input betza file")
        <*> optional
            ( strOption $
                long "dot"
                    <> metavar "FILE|PHASE"
                    <> help "Write AST as dot graph to FILE, or - for stdout; --dot=ps|ds DOTFILE forces a phase"
            )
        <*> optional (argument str (metavar "DOTFILE" <> help "Where --dot=PHASE writes its graph"))
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
  where
    assemble file dot dotFile ws prelude verb flags =
        (\request -> Options file request ws prelude verb flags) <$> reconcileDot dot dotFile

{- | @--dot@ carries either the path itself or a phase whose path follows it, so the
one option covers both forms without a second flag.
-}
reconcileDot :: Maybe String -> Maybe FilePath -> Either String (Maybe DotRequest)
reconcileDot Nothing Nothing = Right Nothing
reconcileDot Nothing (Just stray) =
    Left ("Unexpected argument `" ++ stray ++ "': a second file is only read after --dot=ps|ds")
reconcileDot (Just given) dotFile = case (parsePhase given, dotFile) of
    (Just phase, Just path) -> Right (Just (DotRequest path (Just phase)))
    (Just phase, Nothing) -> Left ("--dot=" ++ show phase ++ " needs a file to write to")
    (Nothing, Nothing) -> Right (Just (DotRequest given Nothing))
    (Nothing, Just stray) ->
        Left ("Unexpected argument `" ++ stray ++ "': --dot already names the file `" ++ given ++ "'")

parsePhase :: String -> Maybe AstPhase
parsePhase "ps" = Just PhasePs
parsePhase "ds" = Just PhaseDs
parsePhase _ = Nothing

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
