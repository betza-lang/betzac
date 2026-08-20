{-# LANGUAGE OverloadedStrings #-}

module Compilation.ScopeSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaProgram)
import Betzac.Compilation.Context (ExportedDef (..), ResolvedDef (..), UsingTarget (..), edIsOverride)
import Betzac.Compilation.Label.Scope (ImportedScope (..), LabelTable, effectiveScope, exportedScope, localDefs, unexportedLabelRefs)
import Betzac.Diagnostic (
    SemanticProblem,
    SemanticProblemKind (DuplicateDirective, DuplicateLabel, UnnecessaryOverride, UnresolvedLabel),
    causeOf,
    semKind,
 )
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
scopeOf self prog = fst $ effectiveScope self (localDefs prog) [] Nothing

-- | A snippet's exported scope, as pulled in by a @using@ of it under the given name.
usingDepAs :: FilePath -> Bool -> Text -> ImportedScope
usingDepAs path overriding src =
    ImportedScope (UsingTarget path overriding Generated) $
        Map.fromList [(edLabel d, d) | d <- fst (exportedScope (scopeOf path prog) prog)]
  where
    prog = parseProgram src

-- | A snippet's exported scope, as pulled in by a @using@ of it named @dep@.
usingDep :: Bool -> Text -> ImportedScope
usingDep = usingDepAs "dep"

causes :: [SemanticProblem] -> [String]
causes = map (causeOf . semKind)

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
                eff = fst $ effectiveScope "main" (localDefs prog) [usingDep False "export N = :2,1:;\n"] Nothing
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

    describe "unexportedLabelRefs" $ do
        it "lists a bare label reference made outside an export" $ do
            let prog = parseProgram "W = :1,1:;\nW;\n"
            map snd (unexportedLabelRefs prog) `shouldBe` ["W"]

        it "skips an exported bare reference, which exportedScope republishes instead" $ do
            let prog = parseProgram "W = :1,1:;\nexport W;\n"
            length (unexportedLabelRefs prog) `shouldBe` 0

    describe "effectiveScope" $ do
        it "prefers override over plain regardless of lexical order" $ do
            let prog = parseProgram "A = fW;\noverride A = fF;\n"
                (resolved, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) `shouldBe` Just True
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "picks the earliest definition among equal precedence" $ do
            let prog = parseProgram "A = fW;\nA = fF;\n"
                (resolved, _probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            fmap (edOrder . rdDef) (Map.lookup "A" resolved) `shouldBe` Just 0

        it "promotes every export from an overriding using to override-class" $ do
            let prog = parseProgram "N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep True "export N = fW;\n"] Nothing
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "dep"
            -- A plain import would have outranked the local plain by itself, so the
            -- override on the using changes nothing.
            causes probs `shouldBe` [causeOf DuplicateLabel, causeOf UnnecessaryOverride]

        it "suppresses the duplicate-label warning for an imported loser when the winner is override-class" $ do
            let prog = parseProgram "override N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep False "export N = fW;\n"] Nothing
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "main"
            length probs `shouldBe` 0

        it "prefers a plain import over a plain local definition of the same label" $ do
            let prog = parseProgram "N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep False "export N = fW;\n"] Nothing
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "dep"
            map (causeOf . semKind) probs `shouldBe` [causeOf DuplicateLabel]

        it "prefers a local override over an import pulled in via override using" $ do
            let prog = parseProgram "override N = fF;\n"
                (resolved, probs) = effectiveScope "main" (localDefs prog) [usingDep True "export N = fW;\n"] Nothing
            fmap rdFrom (Map.lookup "N" resolved) `shouldBe` Just "main"
            -- The using won nothing, so its own override earned nothing either.
            causes probs `shouldBe` [causeOf UnnecessaryOverride]

    describe "unnecessary override" $ do
        it "flags a local override with nothing to outrank" $ do
            let prog = parseProgram "override A = fW;\n"
                (_, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            causes probs `shouldBe` [causeOf UnnecessaryOverride]

        it "flags a local override that already came first" $ do
            let prog = parseProgram "override A = fW;\nA = fF;\n"
                (_, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            causes probs `shouldBe` [causeOf DuplicateLabel, causeOf UnnecessaryOverride]

        it "stays quiet when the override is what beats an earlier plain definition" $ do
            let prog = parseProgram "A = fW;\noverride A = fF;\n"
                (_, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            causes probs `shouldBe` [causeOf DuplicateLabel]

        it "does not flag a losing override, only the selected one" $ do
            let prog = parseProgram "override A = fW;\noverride A = fF;\n"
                (_, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
            causes probs `shouldBe` [causeOf DuplicateLabel]

        it "stays quiet when demoting would lose to an import on class rather than position" $ do
            let prog = parseProgram "override N = fF;\n"
                (_, probs) = effectiveScope "main" (localDefs prog) [usingDep False "export N = fW;\n"] Nothing
            length probs `shouldBe` 0

        it "flags an override using once, not once per label it provides" $ do
            let prog = parseProgram "export A;\n"
                dep = usingDep True "export A = fW;\nexport B = fF;\nexport C = fA;\n"
                (_, probs) = effectiveScope "main" (localDefs prog) [dep] Nothing
            causes probs `shouldBe` [causeOf UnnecessaryOverride]

        it "spares an override using when even one of its labels needed the promotion" $ do
            let prog = parseProgram "export N;\n"
                plain = usingDepAs "first" False "export N = fW;\n"
                overriding = usingDepAs "second" True "export N = fF;\nexport M = fA;\n"
                (_, probs) = effectiveScope "main" (localDefs prog) [plain, overriding] Nothing
            length probs `shouldBe` 0

        it "flags an override using that only wins what its position alone would have" $ do
            let prog = parseProgram "export N;\n"
                overriding = usingDepAs "first" True "export N = fW;\n"
                plain = usingDepAs "second" False "export N = fF;\n"
                (_, probs) = effectiveScope "main" (localDefs prog) [overriding, plain] Nothing
            causes probs `shouldBe` [causeOf UnnecessaryOverride]

    describe "priority resolution property" $
        it "always selects an override-class candidate over any plain candidate, and the earliest among ties" $
            hedgehog $ do
                flags <- forAll $ Gen.list (Range.linear 1 6) Gen.bool
                let src = T.concat [(if ov then "override " else "") <> "A = fW;\n" | ov <- flags]
                    prog = parseProgram src
                    (resolved, probs) = effectiveScope "<test>" (localDefs prog) [] Nothing
                    expectedWinnerIx = case [i | (i, True) <- zip [0 :: Int ..] flags] of
                        (i : _) -> i
                        [] -> 0
                    -- The sole override, placed first, would have won on position alone.
                    onlyLeadingOverride = case flags of
                        (True : rest) -> not (or rest)
                        _ -> False
                    countOf c = length $ filter (== causeOf c) (causes probs)
                annotate (show flags)
                countOf DuplicateLabel === length flags - 1
                countOf UnnecessaryOverride === (if onlyLeadingOverride then 1 else 0)
                fmap (edOrder . rdDef) (Map.lookup "A" resolved) === Just expectedWinnerIx
                fmap (edIsOverride . rdDef) (Map.lookup "A" resolved) === Just (or flags)
