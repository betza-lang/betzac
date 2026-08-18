module Betzac.Compilation.Diagnostic (
    Diagnostic (..),
    infoDiagnostic,
    diagCause,
    diagLine,
    RenderedDiagnostic (..),
    renderDiagnostic,
) where

import Betzac.Diagnostic (
    SemanticProblem (..),
    SemanticProblemKind (CompilationSucceeded),
    Severity (Info),
    causeOf,
    mkProblem,
 )

import Betzac.Span (Span (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Pos (SourcePos (..), unPos)

{- | A single compilation log. 'diagFile' is 'Nothing' for job-level logs that belong
to no one file, like the success info log.
-}
data Diagnostic = Diagnostic
    { diagFile :: Maybe FilePath
    , diagProblem :: SemanticProblem
    }
    deriving (Show)

-- | The job-level info log produced when compilation completes with no errors.
infoDiagnostic :: Diagnostic
infoDiagnostic = Diagnostic Nothing (mkProblem Info CompilationSucceeded Generated)

diagCause :: Diagnostic -> String
diagCause = causeOf . semKind . diagProblem

diagLine :: Diagnostic -> Maybe Int
diagLine d = case semSpan (diagProblem d) of
    RealSpan (SourcePos _ l _) _ -> Just (unPos l)
    Generated -> Nothing

data RenderedDiagnostic = RenderedDiagnostic
    { rdFile :: Maybe FilePath
    , rdLine :: Maybe Int
    , rdCause :: String
    , rdDescription :: Maybe Text
    -- ^ Source snippet, resolved via source-text lookup
    }
    deriving (Show)

renderDiagnostic :: (FilePath -> Maybe Text) -> Diagnostic -> RenderedDiagnostic
renderDiagnostic lookupSource d =
    RenderedDiagnostic
        { rdFile = diagFile d
        , rdLine = diagLine d
        , rdCause = diagCause d
        , rdDescription = snippet
        }
  where
    snippet = do
        fp <- diagFile d
        ln <- diagLine d
        src <- lookupSource fp
        let ls = T.lines src
        if ln >= 1 && ln <= length ls
            then Just (ls !! (ln - 1))
            else Nothing
