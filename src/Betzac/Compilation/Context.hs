module Betzac.Compilation.Context (
    CompilationContext (..),
    FileEntry (..),
    FileStatus (..),
    UsingTarget (..),
    ExportedDef (..),
    edIsOverride,
    edExpr,
    ScopeDef,
    localDef,
    importedDef,
    sdEntry,
    sdExpr,
    sdIsOverride,
    ResolvedDef (..),
    rdOrigin,
    feDiagnostics,
    emptyContext,
) where

import Control.Monad ((>=>))
import qualified Data.Map.Strict as Map

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaExpr, Labelling, QualifiedStmt)
import Betzac.AST.Utils (exprOf, isOverride, stmtOf)
import Betzac.Compilation.Flag (CompilerOptions)
import Betzac.Compilation.Interface (InterfaceEntry, ieOrigin, interfaceEntry)
import Betzac.Diagnostic (SemanticProblem)
import Betzac.Pipeline (PipelineResult)
import Betzac.Span (HasSpan (..), Span)

-- | One @using@ directive, once its target has been resolved to a real file.
data UsingTarget = UsingTarget
    { usingPath :: FilePath
    , usingIsOverride :: Bool
    , usingSpan :: Span
    }
    deriving (Eq, Show)

instance HasSpan UsingTarget where
    getSpan = usingSpan

{- | One label's definition, kept as the whole qualified statement so diagnostics about
it can span the @export@/@override@ keyword too.
-}
data ExportedDef = ExportedDef
    { edLabel :: Labelling
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

{- | A definition as one file's scope sees it. Only a definition in the file being
resolved carries a body; anything reached through an interface is opaque.
-}
data ScopeDef
    = LocalDef InterfaceEntry ExportedDef
    | ImportedDef InterfaceEntry

instance HasSpan ScopeDef where
    getSpan = getSpan . sdEntry

-- | A definition in the file being resolved, seen as that file's own export.
localDef :: FilePath -> ExportedDef -> ScopeDef
localDef file d = LocalDef (interfaceEntry (edLabel d) (edOrder d) file (getSpan d)) d

importedDef :: InterfaceEntry -> ScopeDef
importedDef = ImportedDef

-- | What a dependent would see of this definition.
sdEntry :: ScopeDef -> InterfaceEntry
sdEntry (LocalDef e _) = e
sdEntry (ImportedDef e) = e

{- | Whether an @override@ directive put this definition here. Never true of an import:
an imported definition's precedence comes from the @using@ that carried it.
-}
sdIsOverride :: ScopeDef -> Bool
sdIsOverride (LocalDef _ d) = edIsOverride d
sdIsOverride (ImportedDef _) = False

-- | The expression this definition binds, when its body is on this side of the boundary.
sdExpr :: ScopeDef -> Maybe (BetzaExpr Ps)
sdExpr (LocalDef _ d) = edExpr d
sdExpr (ImportedDef _) = Nothing

-- | A label's winning definition, tagged with the file that contributed it.
data ResolvedDef = ResolvedDef
    { rdFrom :: FilePath
    -- ^ The @using@ target, or the file itself: where this scope got it from.
    , rdDef :: ScopeDef
    }

{- | The file that really defines the label, which is not 'rdFrom' when a dependency
re-exports something it imported.
-}
rdOrigin :: ResolvedDef -> FilePath
rdOrigin = ieOrigin . sdEntry . rdDef

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
    , feExported :: Maybe (Map.Map String InterfaceEntry)
    -- ^ This file's interface: what its dependents are entitled to see.
    , feEffective :: Maybe (Map.Map String ResolvedDef)
    , feDirectiveProblems :: [SemanticProblem]
    -- ^ From discovery: an unresolvable, circular or duplicated @using@.
    , feScopeProblems :: [SemanticProblem]
    -- ^ From scope resolution, kept apart so a reused resolution carries its own.
    , feStatus :: FileStatus
    }

{- | Every problem attributed to the file itself, distinct from 'fePipeline''s own
per-node ones.
-}
feDiagnostics :: FileEntry -> [SemanticProblem]
feDiagnostics entry = feDirectiveProblems entry ++ feScopeProblems entry

data CompilationContext = CompilationContext
    { ccWorkspaceRoot :: FilePath
    , ccTarget :: FilePath
    , ccPrelude :: Maybe FilePath
    -- ^ In scope in every file but the prelude itself, without any directive.
    , ccFiles :: Map.Map FilePath FileEntry
    , ccOptions :: CompilerOptions
    }

emptyContext :: FilePath -> FilePath -> Maybe FilePath -> CompilerOptions -> CompilationContext
emptyContext root target prelude opts =
    CompilationContext
        { ccWorkspaceRoot = root
        , ccTarget = target
        , ccPrelude = prelude
        , ccFiles = Map.empty
        , ccOptions = opts
        }
