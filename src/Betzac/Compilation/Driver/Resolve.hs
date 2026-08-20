module Betzac.Compilation.Driver.Resolve (resolveScopes) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Betzac.Compilation.Context (
    CompilationContext (..),
    ExportedDef (edLabel),
    FileEntry (..),
    FileStatus (ScopesResolved),
    ResolvedDef,
    UsingTarget (usingPath),
 )
import Betzac.Compilation.Label (checkLabels)
import Betzac.Compilation.Label.Scope (ImportedScope (..), effectiveScope, exportedScope, localDefs)
import Betzac.Diagnostic (Stage, logProblems, runStage)
import Betzac.Pipeline (PipelineResult (..))

{- | Compute every discovered file's exported and effective scope, once discovery has
finished. Dependency edges recorded during discovery are guaranteed acyclic (a cycle
is never added to 'feUsingDeps'), so a plain per-file memoized recursion is enough to
process dependencies before dependents — no separate topological sort is needed.
-}
resolveScopes :: CompilationContext -> CompilationContext
resolveScopes ctx0 = foldl' (flip resolveFile) ctx0 $ Map.keys $ ccFiles ctx0

resolveFile :: FilePath -> CompilationContext -> CompilationContext
resolveFile path ctx = case Map.lookup path $ ccFiles ctx of
    Nothing -> ctx
    Just entry -> case feEffective entry of
        Just _ -> ctx
        Nothing ->
            let ctx1 = foldl' (flip resolveFile) ctx $ map usingPath $ feUsingTargets entry
                entry1 = ccFiles ctx1 Map.! path
                (result, probs) = runStage $ resolveFileStage path ctx1
                (exportedMap, effective) = fromMaybe (Map.empty, Map.empty) result

                entry2 =
                    entry1
                        { feExported = Just exportedMap
                        , feEffective = Just effective
                        , feDiagnostics = feDiagnostics entry1 ++ probs
                        , feStatus = ScopesResolved
                        }
             in ctx1{ccFiles = Map.insert path entry2 $ ccFiles ctx1}

{- | The exported/effective scope computation for one file, as a single Stage
sequence, both lifted by logProblems (which never halts the chain). effectiveScope
runs first -- a bare re-export (@export A;@) resolves against the file's *effective*
scope (local or pulled in via @using@), so exportedScope needs it already computed.
-}
resolveFileStage :: FilePath -> CompilationContext -> Stage (Map.Map String ExportedDef, Map.Map String ResolvedDef)
resolveFileStage path ctx1 = do
    let entry1 = ccFiles ctx1 Map.! path
        prog = maybe [] fst (parseResult $ fePipeline entry1)
        localCands = localDefs prog

        imports = [ImportedScope t (exportsOf $ usingPath t) | t <- feUsingTargets entry1]
        exportsOf dp = fromMaybe Map.empty $ feExported =<< Map.lookup dp (ccFiles ctx1)

        (effective, effectiveProbs) = effectiveScope path localCands imports

        (exported, exportProbs) = exportedScope effective prog
        exportedMap = Map.fromList [(edLabel d, d) | d <- exported]

    logProblems exportProbs
    logProblems effectiveProbs
    logProblems (checkLabels effective exportedMap path imports prog)
    pure (exportedMap, effective)
