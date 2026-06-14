module Betzac.Pipeline (
    fromScratch,
    updatePipeline,
    PipelineResult (..),
) where

import Betzac.Lexer.Core (LexError (..), runLexer)
import Betzac.Lexer.Lexer (lexSource)
import Betzac.Token (Token)
import Data.Text (Text, unpack)

updatePipeline :: PipelineResult -> Text -> PipelineResult
updatePipeline _ = fromScratch

fromScratch :: Text -> PipelineResult
fromScratch srcTxt =
    PipelineResult
        { sourceText = srcTxt
        , lexResult = Just $ case runLexer lexSource s of
            Left err -> Left err
            Right (tokens, _, []) -> Right tokens
            Right (_, pos, _) -> Left $ LexError pos
        }
  where
    s = unpack srcTxt

data PipelineResult = PipelineResult
    { sourceText :: Text
    , lexResult :: Maybe (Either LexError [Token])
    -- , astResult :: Maybe (Either ... BetzaExpr)
    }
