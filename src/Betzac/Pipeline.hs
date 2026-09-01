module Betzac.Pipeline (
    fromScratch,
    updatePipeline,
    PipelineResult (..),
    PipelineError (..),
) where

import Betzac.AST.Desugar (desugar)
import Betzac.AST.Phases as B (Ds, Ps)
import Betzac.AST.Types as B (BetzaProgram)
import qualified Betzac.Lexer.Lexer as B (runLexer)
import Betzac.Located
import qualified Betzac.Parser.BetzaTokenStream as B (BetzaTokenStream (..))
import qualified Betzac.Parser.Parser as B (parseTokensRecovering)
import qualified Betzac.Token as B (Token)

import Betzac.Diagnostic (SemanticProblem)
import Betzac.Semantic.Passes (runAllPasses)
import Control.Monad.Trans.State.Strict (StateT, execStateT, gets, modify)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text, unpack)
import Data.Void
import Text.Megaparsec (
    ParseError,
    ParseErrorBundle (..),
    PosState (..),
    defaultTabWidth,
    errorOffset,
    initialPos,
    parse,
 )
import Prelude hiding (length)

type LexBundle = ParseErrorBundle String Void
type ParseBundle = ParseErrorBundle B.BetzaTokenStream Void

type Pipeline a = StateT PipelineResult (Either PipelineError) a

{- | Best-effort: tokens/AST are always present once their stage has run. A
lex or parse failure recovers at the next ';'/statement boundary rather than
aborting the whole file, so later stages always have *something* to work
with. The bundle, when present, carries every error accumulated during
recovery.
-}
data PipelineResult = PipelineResult
    { sourceText :: Text
    , filePath :: FilePath
    , lexResult :: Maybe ([Located B.Token], Maybe LexBundle)
    , parseResult :: Maybe (BetzaProgram Ps, Maybe ParseBundle)
    , desugarResult :: Maybe (BetzaProgram Ds)
    , semanticResult :: Maybe [SemanticProblem]
    }
    deriving (Show)

emptyResult :: FilePath -> Text -> PipelineResult
emptyResult f src =
    PipelineResult
        { sourceText = src
        , filePath = f
        , lexResult = Nothing
        , parseResult = Nothing
        , desugarResult = Nothing
        , semanticResult = Nothing
        }

data PipelineError = SystemError String deriving (Show)

pipeline :: Pipeline ()
pipeline = lexStage >> parseStage >> desugarStage >> semanticStage

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
    case fst <$> mlex of
        Nothing -> return ()
        Just ts -> do
            f <- gets filePath
            let stream = B.BetzaTokenStream src ts
            modify $ \r ->
                r
                    { parseResult = Just $ case parse B.parseTokensRecovering f stream of
                        Left bundle -> ([], Just bundle)
                        Right (prog, errs) -> (prog, toErrorBundle f stream errs)
                    }

toErrorBundle :: FilePath -> B.BetzaTokenStream -> [ParseError B.BetzaTokenStream Void] -> Maybe ParseBundle
toErrorBundle f stream errs = case NE.nonEmpty errs of
    Nothing -> Nothing
    Just ne -> Just $ ParseErrorBundle (NE.sortWith errorOffset ne) initPosState
  where
    initPosState =
        PosState
            { pstateInput = stream
            , pstateOffset = 0
            , pstateSourcePos = initialPos f
            , pstateTabWidth = defaultTabWidth
            , pstateLinePrefix = ""
            }

desugarStage :: Pipeline ()
desugarStage = do
    mparse <- gets parseResult
    case fst <$> mparse of
        Nothing -> return ()
        Just ast -> modify $ \r -> r{desugarResult = Just $ desugar ast}

semanticStage :: Pipeline ()
semanticStage = do
    mparse <- gets parseResult
    mdesugar <- gets desugarResult
    case (fst <$> mparse, mdesugar) of
        (Just ast, Just ds) -> modify $ \r -> r{semanticResult = Just $ runAllPasses ast ds}
        _ -> return ()
