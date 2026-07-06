{-# LANGUAGE OverloadedStrings #-}

module Compilation.ScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram)
import Betzac.Compilation.Context (ExportedDef (..), ResolvedDef (..))
import Betzac.Compilation.Scope (effectiveScope, exportedScope, localDefs)
import Betzac.Pipeline (PipelineResult (parseResult), fromScratch)
import Betzac.Semantic.Core (SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel), causeOf, semKind)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog

-- | Parse a small, known-valid betza source snippet into its top-level program.
-- Fails loudly (via 'error') on any lex/parse failure, since every fixture used here
-- is hand-verified valid — appropriate for a test helper
parseProgram :: Text -> BetzaProgram Ps
parseProgram src = case fromScratch "<test>" src of
    Left e -> error ("pipeline error: " ++ show e)
    Right pr -> case parseResult pr of
        Just (Right prog) -> prog
        Just (Left bundle) -> error ("parse error: " ++ show bundle)
        Nothing -> error "parser did not run"

spec :: Spec
spec = describe "Compilation.Scope" $ do
    describe "exportedScope" $ do
        it "yields one definition per exported label" $ do
            let prog = parseProgram "export A = fW;\nexport B = fF;\n"
                (defs, probs) = exportedScope "export A = fW;\nexport B = fF;\n" prog
            map edLabel defs `shouldBe` ["A", "B"]
            length probs `shouldBe` 0

        it "resolves a bare label-resolving export against the local winner, not a synthetic label" $ do
            let src = "override N = fA;\nN = fW;\nexport N;\n"
                prog = parseProgram src
                (defs, probs) = exportedScope src prog
            map edLabel defs `shouldBe` ["N"]
            map edIsOverride defs `shouldBe` [True]
            length probs `shouldBe` 0

        it "reports an unresolved label for a bare export with no local definition" $ do
            let src = "export Q;\n"
                prog = parseProgram src
                (defs, probs) = exportedScope src prog
            length defs `shouldBe` 0
            map (causeOf . semKind) probs `shouldBe` [causeOf UnresolvedLabel]

        it "keeps only the highest-priority definition for a same-label export repeated in one file, warning on the rest" $ do
            let src = "export A = fW;\noverride export A = fF;\n"
                prog = parseProgram src
                (defs, probs) = exportedScope src prog
            map edIsOverride defs `shouldBe` [True]
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateDirective]

    describe "effectiveScope" $ do
        it "prefers override over plain regardless of lexical order" $ do
            let src = "A = fW;\noverride A = fF;\n"
                prog = parseProgram src
                locals = localDefs src prog
                (resolved, probs) = effectiveScope "<test>" locals []
            fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) `shouldBe` Just True
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "picks the earliest definition among equal precedence" $ do
            let src = "A = fW;\nA = fF;\n"
                prog = parseProgram src
                locals = localDefs src prog
                (resolved, _probs) = effectiveScope "<test>" locals []
            fmap (edOrder . rdDef) (Map.lookup "A" resolved) `shouldBe` Just 0

        it "promotes every export from an overriding using to override-class" $ do
            let depSrc = "export N = fW;\n"
                depProg = parseProgram depSrc
                (depExported, _) = exportedScope depSrc depProg
                depExportedMap = Map.fromList [(edLabel d, d) | d <- depExported]
                src = "N = fF;\n"
                prog = parseProgram src
                locals = localDefs src prog
                (resolved, probs) = effectiveScope "main" locals [("dep", True, depExportedMap)]
            fmap (rdFrom) (Map.lookup "N" resolved) `shouldBe` Just "dep"
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "suppresses the duplicate-label warning for an imported loser when the winner is override-class" $ do
            let depSrc = "export N = fW;\n"
                depProg = parseProgram depSrc
                (depExported, _) = exportedScope depSrc depProg
                depExportedMap = Map.fromList [(edLabel d, d) | d <- depExported]
                src = "override N = fF;\n"
                prog = parseProgram src
                locals = localDefs src prog
                (resolved, probs) = effectiveScope "main" locals [("dep", False, depExportedMap)]
            fmap (rdFrom) (Map.lookup "N" resolved) `shouldBe` Just "main"
            length probs `shouldBe` 0

    describe "priority resolution property" $
        it "always selects an override-class candidate over any plain candidate, and the earliest among ties" $
            hedgehog $ do
                flags <- forAll $ Gen.list (Range.linear 1 6) Gen.bool
                let stmts = [(if ov then "override " else "") <> "A = fW;\n" | ov <- flags]
                    src = T.concat stmts
                    prog = parseProgram src
                    locals = localDefs src prog
                    (resolved, probs) = effectiveScope "<test>" locals []
                    expectedWinnerIx = case [i | (i, True) <- zip [0 :: Int ..] flags] of
                        (i : _) -> i
                        [] -> 0
                annotate (show flags)
                length probs === length flags - 1
                fmap (edOrder . rdDef) (Map.lookup "A" resolved) === Just expectedWinnerIx
                fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) === Just (or flags)