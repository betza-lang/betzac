module Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (..),
    UsingTarget (..),
    ExportedDef (..),
    edIsOverride,
    edExpr,
    ResolvedDef (..),
    emptyContext,
) where

import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaExpr, QualifiedStmt)
import Betzac.AST.Utils (exprOf, isOverride, stmtOf)
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Diagnostic (SemanticProblem)
import Betzac.Pipeline (PipelineResult)
import Betzac.Span (HasSpan (..), Span)

-- | One @using@ directive, once its target has been resolved to a real file.
data UsingTarget = UsingTarget
    { usingPath :: FilePath
    , usingIsOverride :: Bool
    , usingSpan :: Span
    }
    deriving (Show)

instance HasSpan UsingTarget where
    getSpan = usingSpan

{- | One label's definition, kept as the whole qualified statement so diagnostics about
it can span the @export@/@override@ keyword too.
-}
data ExportedDef = ExportedDef
    { edLabel :: String
    , edQualifiedStmt :: QualifiedStmt Ps
    , edOrder :: Int
    -- ^ Lexical position within its file, for tie-breaking.
    }
    deriving (Show)

instance HasSpan ExportedDef where
    getSpan = getSpan . edQualifiedStmt

edIsOverride :: ExportedDef -> Bool
edIsOverride = isOverride . edQualifiedStmt

-- | The expression this definition binds, if it binds one.
edExpr :: ExportedDef -> Maybe (BetzaExpr Ps)
edExpr = (stmtOf >=> exprOf) . edQualifiedStmt

-- | A label's winning definition, tagged with the file that contributed it.
data ResolvedDef = ResolvedDef
    { rdFrom :: FilePath
    , rdDef :: ExportedDef
    }
    deriving (Show)

data FileStatus = Discovered | Parsed | ScopesResolved | Failed
    deriving (Eq, Show)

{- | Everything known about one file discovered during a compilation job. Entries are
keyed by canonicalized absolute path in 'ccFiles', so a file reached by more than one
path is only ever parsed and compiled once.
-}
data FileEntry = FileEntry
    { fePipeline :: PipelineResult
    , feUsingTargets :: [UsingTarget]
    -- ^ In lexical order.
    , feExported :: Maybe (Map.Map String ExportedDef)
    , feEffective :: Maybe (Map.Map String ResolvedDef)
    , feDiagnostics :: [SemanticProblem]
    -- ^ From directive resolution, distinct from 'fePipeline''s own per-node ones.
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
