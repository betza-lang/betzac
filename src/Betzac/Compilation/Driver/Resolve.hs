module Betzac.Compilation.Driver.Resolve (resolveScopes, resolveScopesFrom, dependenciesOf) where

import Data.Foldable (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (ScopesResolved),
    ResolvedDef,
    UsingTarget (usingPath),
 )
import Betzac.Compilation.Interface (InterfaceEntry, ieLabel)
import Betzac.Compilation.Label (checkLabels)
import Betzac.Compilation.Label.Scope (ImportedScope (..), PreludeScope (..), effectiveScope, exportedScope, localDefs)
import Betzac.Diagnostic (Stage, logProblems, runStage)
import Betzac.Pipeline (PipelineResult (..))

{- | Compute every discovered file's exported and effective scope, once discovery has
finished. Dependency edges recorded during discovery are guaranteed acyclic (a cycle
is never added to 'feUsingDeps'), so a plain per-file memoized recursion is enough to
process dependencies before dependents — no separate topological sort is needed.
-}
resolveScopes :: CompilationContext -> CompilationContext
resolveScopes = resolveScopesFrom Nothing

{- | 'resolveScopes', carrying over whatever an earlier context already resolved. A
file's result depends only on its own text and its dependencies' exports, so it can be
taken as-is when its text and @using@ targets are unchanged and every dependency was
itself carried over — the same acyclic ordering the recursion already relies on makes
that an induction rather than a guess. What an editor recompiles per keystroke is then
proportional to the edit, not to the workspace.
-}
resolveScopesFrom :: Maybe CompilationContext -> CompilationContext -> CompilationContext
resolveScopesFrom prev ctx0 =
    rsContext $ foldl' (flip $ resolveFile prev) (Resolution ctx0 Set.empty) $ Map.keys $ ccFiles ctx0

-- | The context being built, plus the files whose resolution was carried over intact.
data Resolution = Resolution
    { rsContext :: CompilationContext
    , rsCarried :: Set.Set FilePath
    }

resolveFile :: Maybe CompilationContext -> FilePath -> Resolution -> Resolution
resolveFile prev path st = case Map.lookup path $ ccFiles (rsContext st) of
    Nothing -> st
    Just entry -> case feEffective entry of
        Just _ -> st
        Nothing ->
            let deps = dependenciesOf (rsContext st) path entry
                st1 = foldl' (flip $ resolveFile prev) st deps
                ctx1 = rsContext st1
                entry1 = ccFiles ctx1 Map.! path
             in case carriedEntry prev (rsCarried st1) deps path entry1 of
                    Just entry2 ->
                        Resolution
                            ctx1{ccFiles = Map.insert path entry2 $ ccFiles ctx1}
                            (Set.insert path $ rsCarried st1)
                    Nothing ->
                        let (result, probs) = runStage $ resolveFileStage path ctx1
                            (exportedMap, effective) = fromMaybe (Map.empty, Map.empty) result
                            entry2 =
                                entry1
                                    { feExported = Just exportedMap
                                    , feEffective = Just effective
                                    , feScopeProblems = probs
                                    , feStatus = ScopesResolved
                                    }
                         in st1{rsContext = ctx1{ccFiles = Map.insert path entry2 $ ccFiles ctx1}}

-- | What a file's scope is computed from: its @using@ targets, and the prelude.
dependenciesOf :: CompilationContext -> FilePath -> FileEntry -> [FilePath]
dependenciesOf ctx path entry =
    map usingPath (feUsingTargets entry) ++ filter (/= path) (toList $ ccPrelude ctx)

{- | 'entry' with an earlier resolution of the same file grafted on, when that
resolution still describes it: same text, same @using@ targets, and every dependency
carried over too, so every export it was computed against is unchanged.
-}
carriedEntry :: Maybe CompilationContext -> Set.Set FilePath -> [FilePath] -> FilePath -> FileEntry -> Maybe FileEntry
carriedEntry prev carried deps path entry = do
    old <- Map.lookup path . ccFiles =<< prev
    exported <- feExported old
    effective <- feEffective old
    if sourceText (fePipeline old) == sourceText (fePipeline entry)
        && feUsingTargets old == feUsingTargets entry
        && all (`Set.member` carried) deps
        then
            Just
                entry
                    { feExported = Just exported
                    , feEffective = Just effective
                    , feScopeProblems = feScopeProblems old
                    , feStatus = ScopesResolved
                    }
        else Nothing

{- | The exported/effective scope computation for one file, as a single Stage
sequence, both lifted by logProblems (which never halts the chain). effectiveScope
runs first -- a bare re-export (@export A;@) resolves against the file's *effective*
scope (local or pulled in via @using@), so exportedScope needs it already computed.
-}
resolveFileStage :: FilePath -> CompilationContext -> Stage (Map.Map String InterfaceEntry, Map.Map String ResolvedDef)
resolveFileStage path ctx1 = do
    let entry1 = ccFiles ctx1 Map.! path
        prog = maybe [] fst (parseResult $ fePipeline entry1)
        localCands = localDefs prog

        imports = [ImportedScope t (exportsOf $ usingPath t) | t <- feUsingTargets entry1]
        exportsOf dp = fromMaybe Map.empty $ feExported =<< Map.lookup dp (ccFiles ctx1)

        -- The prelude is not in scope in itself.
        prelude = do
            p <- ccPrelude ctx1
            if p == path then Nothing else Just (PreludeScope p (exportsOf p))

        (effective, effectiveProbs) = effectiveScope path localCands imports prelude

        (exported, exportProbs) = exportedScope effective prog
        exportedMap = Map.fromList [(ieLabel e, e) | e <- exported]

    logProblems exportProbs
    logProblems effectiveProbs
    logProblems (checkLabels effective exportedMap path imports prog)
    pure (exportedMap, effective)
