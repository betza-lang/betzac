{-# LANGUAGE OverloadedStrings #-}

module LangServer.Handlers.Core (publishDiagnostics, makeDiagnostic) where

import Betzac.Compilation.Context (CompilationContext (..), FileEntry (..))
import Betzac.Compilation.Driver (SourceReader, discover, resolvePrelude, resolveScopes, sealPrelude)
import Betzac.Compilation.Flag (CompilerFlag (..), CompilerOptions, Wspecifier (..), optionsFromFlags)
import Betzac.Debug.PrettyPrint (prettyPrint)
import Betzac.Diagnostic (SemanticProblem (..), Severity (..))
import qualified Betzac.Pipeline as B (PipelineResult (..))
import Betzac.Span (Span (..))

import Text.Megaparsec

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)

import LangServer.Config
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens (HasVersion (version))
import Language.LSP.Protocol.Types
import Language.LSP.Server hiding (publishDiagnostics)
import qualified Language.LSP.Server as LSP (publishDiagnostics)
import Language.LSP.VFS (VFS (_vfsMap), virtualFileText)

{- | bls's -Wall equivalent: the CLI defaults to no warnings at all, but an editor
should surface every optional one — a squiggle is cheap to ignore, a missing one is not.
-}
blsOptions :: CompilerOptions
blsOptions = optionsFromFlags [GenerateWarnings Wunused, GenerateWarnings Wdirective, GenerateWarnings Wlang]

-- | Passed through to the lsp library's own diagnostic store.
maxDiagnostics :: Int
maxDiagnostics = 100

{- | Compile 'fp' (and everything it @using@s, transitively) and publish diagnostics
for every file reached, each against its own URI — matching how established
compiler-backed language servers (clangd, gopls, ...) behave: a problem is published
wherever the compiler actually attributes it, not collapsed onto the file you happen
to have open. Every reached file gets an explicit publish, even an empty one — clients
(and 'Language.LSP.Test.waitForDiagnostics') wait for the notification itself, not
just for it to be non-empty, so a clean file has to be republished too, not skipped.

Deliberately does *not* call 'flushDiagnosticsBySource': it reports whatever's
currently in the lsp library's own diagnostic store, i.e. last call's results, and
since that store isn't updated until we actually publish, calling it before our own
fresh publish sends a stale notification one edit behind — which is exactly what
happened here (three 'Language.LSP.Test.waitForDiagnostics' tests all observed the
previous compile's result, not the latest). Flush is for occasional bulk-clears, not
a per-edit hot path. The tradeoff: a file that drops out of the dependency graph
entirely (e.g. its last @using@ reference gets deleted) keeps showing its last
diagnostics until something else republishes for it — a real but narrow gap, tracked
as a known limitation rather than solved here.
-}
publishDiagnostics :: FilePath -> Uri -> Text -> LspM ConfigBLS ()
publishDiagnostics fp _u src = do
    mRoot <- getRootPath
    let root = fromMaybe (takeDirectory fp) mRoot
    reader <- overlayReader fp src
    result <- liftIO $ runExceptT $ do
        prelude <- ExceptT $ resolvePrelude Nothing
        ctx0 <- ExceptT $ discover reader root fp (Just prelude) blsOptions
        ExceptT $ return $ sealPrelude $ resolveScopes ctx0
    case result of
        -- A system failure (missing workspace root/target) isn't attributable to any
        -- particular file's own text; best-effort attach it to the file being edited
        -- rather than silently show nothing.
        Left problem -> publishFor fp [semanticProblemToDiagnostic problem]
        -- The prelude is never published: it is not the user's source to fix.
        Right ctx ->
            mapM_ (uncurry publishFor . fmap fileDiagnostics) $
                filter ((/= ccPrelude ctx) . Just . fst) $
                    Map.toList $
                        ccFiles ctx
  where
    -- 'notificationHandler's aren't guaranteed to complete in receipt order (the lsp
    -- library may run them concurrently) — without a version, a slower earlier compile
    -- finishing after a faster later one would silently overwrite it on the wire.
    -- Passing the file's current tracked version lets the library's own diagnostic
    -- store discard a late, stale publish instead of applying it.
    publishFor path diags = do
        verIdent <- getVersionedTextDoc (TextDocumentIdentifier (filePathToUri path))
        LSP.publishDiagnostics maxDiagnostics (toNormalizedUri (filePathToUri path)) (Just (verIdent ^. version)) (partitionBySource diags)

{- | A 'SourceReader' that serves every currently-open document's live editor buffer
(via the lsp library's VFS, which 'onOpen'/'onChange' keep current) and falls back to
disk for anything not open. 'fp' is forced to 'src' regardless, removing any doubt
about VFS update ordering relative to the notification that triggered this call.

Keys are canonicalized to match what 'Driver.Discovery' actually looks up with: every
path it ever passes to a 'SourceReader' has already been through 'canonicalizePath'
(the target in 'discover', every resolved @using@ target in 'resolveUsingTarget'), so
an overlay keyed by the raw, not-necessarily-canonical paths 'uriToFilePath' hands back
would silently miss and fall through to disk.
-}
overlayReader :: FilePath -> Text -> LspM ConfigBLS SourceReader
overlayReader fp src = do
    vfs <- getVirtualFiles
    let rawOpened =
            (fp, src)
                : [ (path, virtualFileText vf)
                  | (nuri, vf) <- Map.toList (_vfsMap vfs)
                  , Just path <- [uriToFilePath (fromNormalizedUri nuri)]
                  ]
    overlay <- liftIO $ Map.fromList <$> mapM (\(path, text) -> (\p -> (p, text)) <$> canonicalizePath path) rawOpened
    pure $ \path -> case Map.lookup path overlay of
        Just liveContent -> pure liveContent
        Nothing -> TIO.readFile path

{- | Every diagnostic belonging to one discovered file: its own lex/parse/single-file
semantic-pass results, plus whatever cross-file (scope/label) diagnostics were
attributed to it.
-}
fileDiagnostics :: FileEntry -> [Diagnostic]
fileDiagnostics entry =
    lexDiags (fePipeline entry)
        <> parseDiags (fePipeline entry)
        <> map semanticProblemToDiagnostic (fromMaybe [] (B.semanticResult (fePipeline entry)))
        <> map semanticProblemToDiagnostic (feDiagnostics entry)

lexDiags :: B.PipelineResult -> [Diagnostic]
lexDiags result = maybe [] (foldMap bundleToDiagnostics . snd) (B.lexResult result)

parseDiags :: B.PipelineResult -> [Diagnostic]
parseDiags result = maybe [] (foldMap bundleToDiagnostics . snd) (B.parseResult result)

bundleToDiagnostics ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
    ParseErrorBundle s e -> [Diagnostic]
bundleToDiagnostics bundle =
    [ makeDiagnostic (sourcePosPairToRange sp sp) (T.pack $ parseErrorTextPretty err) DiagnosticSeverity_Error
    | (err, sp) <-
        NE.toList $
            fst $
                attachSourcePos errorOffset (bundleErrors bundle) (bundlePosState bundle)
    ]

semanticProblemToDiagnostic :: SemanticProblem -> Diagnostic
semanticProblemToDiagnostic problem =
    makeDiagnostic
        (toRange . semSpan $ problem)
        (T.pack . prettyPrint . semKind $ problem)
        (getLspSeverity problem)

-- utils

sourcePosPairToRange :: SourcePos -> SourcePos -> Range
sourcePosPairToRange spos epos =
    let s = sourcePosToPosition spos
        e = sourcePosToPosition epos
     in Range s e

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition sp =
    Position
        (fromIntegral $ unPos (sourceLine sp) - 1)
        (fromIntegral $ unPos (sourceColumn sp) - 1)

makeDiagnostic :: Range -> Text -> DiagnosticSeverity -> Diagnostic
makeDiagnostic range message severity =
    Diagnostic range (Just severity) Nothing Nothing (Just "betzac") message Nothing Nothing Nothing

-- Note: Rangeless diagnostics are currently unsupported in the LSP spec
--       See https://github.com/microsoft/language-server-protocol/issues/256
toRange :: Span -> Range
toRange Generated = Range (Position 0 0) (Position 0 0)
toRange (RealSpan s e) = sourcePosPairToRange s e

getLspSeverity :: SemanticProblem -> DiagnosticSeverity
getLspSeverity (SemanticProblem Info _ _) = DiagnosticSeverity_Information
getLspSeverity (SemanticProblem Warning _ _) = DiagnosticSeverity_Warning
getLspSeverity (SemanticProblem Error _ _) = DiagnosticSeverity_Error
