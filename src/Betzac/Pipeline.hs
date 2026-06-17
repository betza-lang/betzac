module Betzac.Pipeline (
    fromScratch,
    updatePipeline,
    PipelineResult (..),
    PipelineError (..),
) where

import Betzac.AST (BetzaProgram)
import Betzac.Lexer.Core (LexError (..), runLexer)
import qualified Betzac.Lexer.Lexer as Lexer (lexSource)
import Betzac.Parser.Core (ParseError, runParser)
import qualified Betzac.Parser.Parser as Parser (parseTokens)
import Betzac.Token (Token)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify)
import Data.Text (Text, unpack)
import Prelude hiding (length)

type Pipeline a = StateT PipelineResult (Either PipelineError) a

data PipelineResult = PipelineResult
    { sourceText :: Text
    , lexResult :: Maybe (Either LexError [Token])
    , parseResult :: Maybe (Either ParseError BetzaProgram)
    }
    deriving (Show)

emptyResult :: Text -> PipelineResult
emptyResult src =
    PipelineResult
        { sourceText = src
        , lexResult = Nothing
        , parseResult = Nothing
        }

data PipelineError
    = SystemError String
    deriving (Show)

pipeline :: Pipeline ()
pipeline = do
    lexSource
    parseTokens

updatePipeline :: PipelineResult -> Text -> Either PipelineError PipelineResult
updatePipeline _ = fromScratch

fromScratch :: Text -> Either PipelineError PipelineResult
fromScratch src = execStateT pipeline (emptyResult src)

lexSource :: Pipeline ()
lexSource = do
    src <- gets $ unpack . sourceText
    modify $ \r ->
        r
            { lexResult = Just $ (\(tokens, _, _) -> tokens) <$> runLexer Lexer.lexSource src
            }

parseTokens :: Pipeline ()
parseTokens = do
    mts <- gets lexResult
    case mts >>= either (const Nothing) Just of
        Just ts -> modify $ \r ->
            r
                { parseResult = Just $ (\(program, _, _) -> program) <$> runParser Parser.parseTokens ts
                }
        Nothing -> pure ()
