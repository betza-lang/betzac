module Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (..),
    ExportedDef (..),
    ResolvedDef (..),
    emptyContext,
) where

import qualified Data.Map.Strict as Map

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaStmt)
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Pipeline (PipelineResult)
import Betzac.Diagnostic (SemanticProblem)

-- | One label's exported definition within a single file's exported scope.
data ExportedDef = ExportedDef
    { edLabel :: String
    , edStmt :: BetzaStmt Ps
    , edIsOverride :: Bool
    , edOrder :: Int
    {- ^ Lexical position of the exporting statement within its file, used as the
    tie-breaker among candidates of equal precedence.
    -}
    }
    deriving (Show)

{- | A label's winning definition after applying resolution priority, tagged
with the file that contributed it.
-}
data ResolvedDef = ResolvedDef
    { rdFrom :: FilePath
    , rdDef :: ExportedDef
    }
    deriving (Show)

data FileStatus = Discovered | Parsed | ScopesResolved | Failed
    deriving (Eq, Show)

{- | Everything known about one file discovered during a compilation job. A single
'FileEntry' is shared by every file that depends on it (keyed by canonicalized
absolute path in 'ccFiles'), so a file discovered as a dependency more than once
is only ever parsed/compiled once.
-}
data FileEntry = FileEntry
    { fePipeline :: PipelineResult
    , feUsingDeps :: [FilePath]
    -- ^ Resolved absolute paths of this file's @using@ targets, in lexical order.
    , feOverrideUsingDeps :: [FilePath]
    -- ^ Subset of 'feUsingDeps' that came from an @override using@ directive.
    , feExported :: Maybe (Map.Map String ExportedDef)
    , feEffective :: Maybe (Map.Map String ResolvedDef)
    , feDiagnostics :: [SemanticProblem]
    {- ^ Diagnostics produced while resolving this file's directives (e.g. @using
    unknown@, @using circular@), distinct from 'fePipeline''s per-node semantic
    diagnostics.
    -}
    , feStatus :: FileStatus
    }

data CompilationContext = CompilationContext
    { ccWorkspaceRoot :: FilePath
    , ccTarget :: FilePath
    , ccFiles :: Map.Map FilePath FileEntry
    , ccOptions :: CompilerOptions
    }

emptyContext :: FilePath -> FilePath -> CompilerOptions -> CompilationContext
emptyContext root target opts =
    CompilationContext
        { ccWorkspaceRoot = root
        , ccTarget = target
        , ccFiles = Map.empty
        , ccOptions = opts
        }
