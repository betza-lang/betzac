module Options (Options, inputFile, emitDot, getOptions) where

import Options.Applicative hiding (Parser)
import qualified Options.Applicative as O (Parser)

data Options = Options
    { inputFile :: FilePath
    , emitDot :: Maybe FilePath
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
                long "emit-dot"
                    <> metavar "FILE"
                    <> value "/dev/null"
                    <> showDefault
                    <> help "Write AST as dot graph to FILE, or - for stdout"
            )
