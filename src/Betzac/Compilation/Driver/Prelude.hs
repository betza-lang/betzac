{- | Locating the standard prelude, which is in scope in every betza file without any
directive naming it.
-}
module Betzac.Compilation.Driver.Prelude (resolvePrelude, sealPrelude) where

import qualified Data.Map.Strict as Map
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import Betzac.Compilation.Context (CompilationContext (..), FileBody (..), FileEntry (..), feHasError)
import Betzac.Diagnostic (
    SemanticProblem (..),
    SemanticProblemKind (SystemFailure),
    Severity (Error),
    mkProblem,
 )
import Betzac.Pipeline (PipelineResult (..))
import Betzac.Span (Span (Generated))
import Paths_betzac (getDataDir)

-- | The prelude's location within the installed data directory.
preludeDataPath :: FilePath
preludeDataPath = "prelude" </> "std.betza"

{- | Locate the standard prelude: an explicitly given path, else @BETZAC_PRELUDE@, else
the installed data directory. A named path that does not exist is an error rather than
a reason to fall through, so a typo never silently compiles against the shipped one.
-}
resolvePrelude :: Maybe FilePath -> IO (Either SemanticProblem FilePath)
resolvePrelude explicit = do
    fromEnv <- lookupEnv "BETZAC_PRELUDE"
    case explicit `orElse` fromEnv of
        Just named -> require named
        Nothing -> require . (</> preludeDataPath) =<< getDataDir
  where
    orElse (Just p) _ = Just p
    orElse Nothing p = p

    require path = do
        exists <- doesFileExist path
        if exists
            then Right <$> canonicalizePath path
            else return $ Left $ systemError $ "standard prelude not found: " ++ path

{- | Settle the prelude's own diagnostics once every scope is resolved. The prelude is
not source the user can act on: an error in it means the installation is broken, and
its warnings are nobody's to fix.
-}
sealPrelude :: CompilationContext -> Either SemanticProblem CompilationContext
sealPrelude ctx = case ccPrelude ctx >>= \p -> (,) p <$> Map.lookup p (ccFiles ctx) of
    Nothing -> Right ctx
    Just (path, entry)
        | feHasError entry -> Left $ systemError $ "standard prelude failed to compile: " ++ path
        | otherwise -> Right ctx{ccFiles = Map.insert path (silenced entry) (ccFiles ctx)}
  where
    silenced entry =
        entry
            { feDirectiveProblems = []
            , feScopeProblems = []
            , feBody = silencedBody (feBody entry)
            }

    -- An interfaced prelude never carried diagnostics in the first place.
    silencedBody (Compiled pr) = Compiled pr{semanticResult = fmap (const []) (semanticResult pr)}
    silencedBody body = body

systemError :: String -> SemanticProblem
systemError msg = mkProblem Error (SystemFailure msg) Generated
