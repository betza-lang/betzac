module Options (
    Options,
    Verbosity (..),
    inputFile,
    emitDot,
    verbosity,
    getOptions,
) where

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O (Parser)

data Verbosity = Silent | Verbose | VeryVerbose deriving (Eq, Ord)

data Options = Options
    { inputFile :: FilePath
    , emitDot :: Maybe FilePath
    , verbosity :: Verbosity
    }

getOptions :: IO Options
getOptions =
    execParser $
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Compile a betza file"
                <> header "betzac - the betza compiler"
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
        <*> verbosityParser

verbosityParser :: O.Parser Verbosity
verbosityParser = toVerbosity <$> many (flag' () (short 'v' <> long "verbose" <> help "Verbosity (-v or -vv)"))
  where
    toVerbosity [] = Silent
    toVerbosity [_] = Verbose
    toVerbosity _ = VeryVerbose
