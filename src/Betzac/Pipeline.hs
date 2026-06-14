module Betzac.Pipeline (
    fromScratch,
    updatePipeline,
    PipelineResult (..),
    PipelineError (..),
) where

import Betzac.Lexer.Core (LexError (..), runLexer)
import qualified Betzac.Lexer.Lexer as Lexer (lexSource)
import Betzac.Token (Token)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify)
import Data.Text (Text, unpack)
import Prelude hiding (length)

type Pipeline a = StateT PipelineResult (Either PipelineError) a

data PipelineResult = PipelineResult
    { sourceText :: Text
    , lexResult :: Maybe (Either LexError [Token])
    -- , astResult :: Maybe (Either ... BetzaExpr)
    }
    deriving (Show)

emptyResult :: Text -> PipelineResult
emptyResult src =
    PipelineResult
        { sourceText = src
        , lexResult = Nothing
        }

-- TODO
-- ParseFailed ParseError
-- AnalysisFailed AnalysisError
data PipelineError
    = SystemError String
    deriving (Show)

pipeline :: Pipeline ()
pipeline = do
    lexSource

lexSource :: Pipeline ()
lexSource = do
    src <- gets $ unpack . sourceText
    modify $ \r ->
        r
            { lexResult = Just $ (\(tokens, _, _) -> tokens) <$> runLexer Lexer.lexSource src
            }

updatePipeline :: PipelineResult -> Text -> Either PipelineError PipelineResult
updatePipeline _ = fromScratch

fromScratch :: Text -> Either PipelineError PipelineResult
fromScratch src = execStateT pipeline (emptyResult src)
