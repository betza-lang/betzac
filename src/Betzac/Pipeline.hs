module Betzac.Pipeline (
    fromScratch,
    updatePipeline,
    PipelineResult (..),
    PipelineError (..),
) where

import Betzac.AST as B (BetzaProgram)
import qualified Betzac.Lexer.Lexer as B (runLexer)
import Betzac.Located
import qualified Betzac.Parser.BetzaTokenStream as B (BetzaTokenStream (..))
import qualified Betzac.Parser.Parser as B (parseTokens)
import qualified Betzac.Token as B (Token)

import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify)
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec
import Prelude hiding (length)

type LexBundle = ParseErrorBundle String Void
type ParseBundle = ParseErrorBundle B.BetzaTokenStream Void

type Pipeline a = StateT PipelineResult (Either PipelineError) a

data PipelineResult = PipelineResult
    { sourceText :: Text
    , filePath :: FilePath
    , lexResult :: Maybe (Either LexBundle [Located B.Token])
    , parseResult :: Maybe (Either ParseBundle BetzaProgram)
    }
    deriving (Show)

emptyResult :: FilePath -> Text -> PipelineResult
emptyResult f src =
    PipelineResult
        { sourceText = src
        , filePath = f
        , lexResult = Nothing
        , parseResult = Nothing
        }

data PipelineError = SystemError String deriving (Show)

pipeline :: Pipeline ()
pipeline = lexStage >> parseStage

updatePipeline :: PipelineResult -> Text -> Either PipelineError PipelineResult
updatePipeline r = fromScratch $ filePath r

fromScratch :: FilePath -> Text -> Either PipelineError PipelineResult
fromScratch f src = execStateT pipeline (emptyResult f src)

lexStage :: Pipeline ()
lexStage = do
    f <- gets filePath
    src <- gets $ unpack . sourceText
    modify $ \r -> r{lexResult = Just $ B.runLexer f src}

parseStage :: Pipeline ()
parseStage = do
    src <- gets $ unpack . sourceText
    mlex <- gets lexResult
    case mlex >>= either (const Nothing) Just of
        Nothing -> return ()
        Just ts -> do
            let stream = B.BetzaTokenStream src ts
            modify $ \r ->
                r
                    { parseResult = Just $ parse B.parseTokens (filePath r) stream
                    }
