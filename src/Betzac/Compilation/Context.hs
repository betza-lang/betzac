module Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (..),
    ExportedDef (..),
    edStmt,
    ResolvedDef (..),
    emptyContext,
) where

import qualified Data.Map.Strict as Map

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaStmt, Directive (..), QualifiedStmt (..))
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Diagnostic (SemanticProblem)
import Betzac.Pipeline (PipelineResult)
import Betzac.Span (HasSpan (..))

{- | One label's exported definition within a single file's exported scope. Stores the
*whole* qualified statement (not just its inner 'BetzaStmt') so diagnostics about the
statement as a whole (duplicate directive/label, unused label) can span the entire
thing — including an @export@/@override@ keyword — rather than just the assignment
or bare reference inside it. Use 'edStmt' to reach the inner statement.
-}
data ExportedDef = ExportedDef
    { edLabel :: String
    , edQualifiedStmt :: QualifiedStmt Ps
    , edIsOverride :: Bool
    , edOrder :: Int
    {- ^ Lexical position of the exporting statement within its file, used as the
    tie-breaker among candidates of equal precedence.
    -}
    }
    deriving (Show)

{- | The span of the whole qualified statement — see 'edQualifiedStmt'. For the inner
statement's own (narrower) span instead, use @getSpan . edStmt@.
-}
instance HasSpan ExportedDef where
    getSpan = getSpan . edQualifiedStmt

{- | The inner statement an 'ExportedDef' wraps. 'edQualifiedStmt' is always a
statement-bearing directive (@Bare@ or @Export@, never @Using@), since only those ever
produce an 'ExportedDef' in the first place (cf. 'Betzac.Compilation.Label.Scope.statementOf').
-}
edStmt :: ExportedDef -> BetzaStmt Ps
edStmt def = case edQualifiedStmt def of
    Override (Bare stmt _) _ -> stmt
    Plain (Bare stmt _) _ -> stmt
    Override (Export stmt _) _ -> stmt
    Plain (Export stmt _) _ -> stmt
    _ -> error "edStmt: ExportedDef unexpectedly wrapped a using directive"

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
