{-# LANGUAGE OverloadedStrings #-}

module Compilation.ScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram)
import Betzac.Compilation.Context (ExportedDef (..), ResolvedDef (..), UsingTarget (..), edIsOverride)
import Betzac.Compilation.Label.Scope (ImportedScope (..), LabelTable, checkLabelRefs, effectiveScope, exportedScope, localDefs)
import Betzac.Diagnostic (SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnresolvedLabel, UnusedLabel), causeOf, semKind)
import Betzac.Pipeline (PipelineResult (parseResult), fromScratch)
import Betzac.Span (Span (Generated))

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog

{- | Parse a small, known-valid betza source snippet into its top-level program.
Fails loudly (via 'error') on any lex/parse failure, since every fixture used here
is hand-verified valid — appropriate for a test helper
-}
parseProgram :: Text -> BetzaProgram Ps
parseProgram src = case fromScratch "<test>" src of
    Left e -> error ("pipeline error: " ++ show e)
    Right pr -> case parseResult pr of
        Just (prog, Nothing) -> prog
        Just (_, Just bundle) -> error ("parse error: " ++ show bundle)
        Nothing -> error "parser did not run"

-- | The effective scope of a program that imports nothing.
scopeOf :: FilePath -> BetzaProgram Ps -> LabelTable ResolvedDef
scopeOf self prog = fst $ effectiveScope self (localDefs prog) []

-- | A snippet's exported scope, as pulled in by a @using@ of it named @dep@.
usingDep :: Bool -> Text -> ImportedScope
usingDep overriding src =
    ImportedScope (UsingTarget "dep" overriding Generated) $
        Map.fromList [(edLabel d, d) | d <- fst (exportedScope (scopeOf "dep" prog) prog)]
  where
    prog = parseProgram src

spec :: Spec
spec = describe "Compilation.Scope" $ do
    describe "exportedScope" $ do
        it "yields one definition per exported label" $ do
            let prog = parseProgram "export A = fW;\nexport B = fF;\n"
                (defs, probs) = exportedScope (scopeOf "<test>" prog) prog
            map edLabel defs `shouldBe` ["A", "B"]
            length probs `shouldBe` 0

        it "resolves a bare label-resolving export against the effective winner, not a synthetic label" $ do
            let prog = parseProgram "override N = fA;\nN = fW;\nexport N;\n"
                (defs, probs) = exportedScope (scopeOf "<test>" prog) prog
            map edLabel defs `shouldBe` ["N"]
            map edIsOverride defs `shouldBe` [True]
            length probs `shouldBe` 0

        it "resolves a bare label-resolving export against a label pulled in via using, not just local definitions" $ do
            let prog = parseProgram "using dep;\nexport N;\n"
                eff = fst $ effectiveScope "main" (localDefs prog) [usingDep False "export N = :2,1:;\n"]
                (defs, probs) = exportedScope eff prog
            map edLabel defs `shouldBe` ["N"]
            length probs `shouldBe` 0

        it "reports an unresolved label for a bare export with no definition anywhere in scope" $ do
            let prog = parseProgram "export Q;\n"
                (defs, probs) = exportedScope (scopeOf "<test>" prog) prog
            length defs `shouldBe` 0
            map (causeOf . semKind) probs `shouldBe` [causeOf UnresolvedLabel]

        it "keeps only the highest-priority definition for a same-label export repeated in one file, warning on the rest" $ do
            let prog = parseProgram "export A = fW;\noverride export A = fF;\n"
                (defs, probs) = exportedScope (scopeOf "<test>" prog) prog
            map edIsOverride defs `shouldBe` [True]
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateDirective]

        it "publishes the effective winner for a direct assign export, not the exporting statement's own body" $ do
            let prog = parseProgram "export A = fW;\noverride A = fF;\n"
                (defs, probs) = exportedScope (scopeOf "<test>" prog) prog
            map edLabel defs `shouldBe` ["A"]
            map edIsOverride defs `shouldBe` [True]
            map edOrder defs `shouldBe` [1] -- the override statement, not the export statement (order 0)
            length probs `shouldBe` 0

        it "treats `export A = expr;` as sugar for `export A; A = expr;`, publishing identically either way" $ do
            let exportOf prog = exportedScope (scopeOf "<test>" prog) prog
                (sugarDefs, sugarProbs) = exportOf $ parseProgram "export A = fW;\noverride A = fF;\n"
                (desugarDefs, desugarProbs) = exportOf $ parseProgram "A = fW;\nexport A;\noverride A = fF;\n"
            map edLabel sugarDefs `shouldBe` map edLabel desugarDefs
            map edIsOverride sugarDefs `shouldBe` map edIsOverride desugarDefs
            length sugarProbs `shouldBe` length desugarProbs

    describe "checkLabelRefs" $ do
        it "flags an unresolved label for an unexported bare reference to an undefined label" $ do
            let prog = parseProgram "Q;\n"
            map (causeOf . semKind) (checkLabelRefs (scopeOf "<test>" prog) prog)
                `shouldBe` [causeOf UnresolvedLabel]

        it "flags an unexported bare reference to a label that does resolve as unused" $ do
            let prog = parseProgram "W = :1,1:;\nW;\n"
            map (causeOf . semKind) (checkLabelRefs (scopeOf "<test>" prog) prog)
                `shouldBe` [causeOf (UnusedLabel "W")]

        it "does not flag an exported bare reference, since exportedScope handles those instead" $ do
            let prog = parseProgram "W = :1,1:;\nexport W;\n"
            length (checkLabelRefs (scopeOf "<test>" prog) prog) `shouldBe` 0

    describe "effectiveScope" $ do
        it "prefers override over plain regardless of lexical order" $ do
            let prog = parseProgram "A = fW;\noverride A = fF;\n"
                (resolved, probs) = effectiveScope "<test>" (localDefs prog) []
            fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) `shouldBe` Just True
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "picks the earliest definition among equal precedence" $ do
            let prog = parseProgram "A = fW;\nA = fF;\n"
                (resolved, _probs) = effectiveScope "<test>" (localDefs prog) []
            fmap (edOrder . rdDef) (Map.lookup "A" resolved) `shouldBe` Just 0

        it "promotes every export from an overriding using to override-class" $ do
            let prog = parseProgram "N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep True "export N = fW;\n"]
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "dep"
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "suppresses the duplicate-label warning for an imported loser when the winner is override-class" $ do
            let prog = parseProgram "override N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep False "export N = fW;\n"]
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "main"
            length probs `shouldBe` 0

        it "prefers a plain import over a plain local definition of the same label" $ do
            let prog = parseProgram "N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep False "export N = fW;\n"]
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "dep"
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "prefers a local override over an import pulled in via override using" $ do
            let prog = parseProgram "override N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep True "export N = fW;\n"]
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "main"
            length probs `shouldBe` 0

    describe "priority resolution property" $
        it "always selects an override-class candidate over any plain candidate, and the earliest among ties" $
            hedgehog $ do
                flags <- forAll $ Gen.list (Range.linear 1 6) Gen.bool
                let src = T.concat [(if ov then "override " else "") <> "A = fW;\n" | ov <- flags]
                    prog = parseProgram src
                    (resolved, probs) = effectiveScope "<test>" (localDefs prog) []
                    expectedWinnerIx = case [i | (i, True) <- zip [0 :: Int ..] flags] of
                        (i : _) -> i
                        [] -> 0
                annotate (show flags)
                length probs === length flags - 1
                fmap (edOrder . rdDef) (Map.lookup "A" resolved) === Just expectedWinnerIx
                fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) === Just (or flags)
