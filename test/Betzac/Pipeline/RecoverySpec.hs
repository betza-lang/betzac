{-# LANGUAGE OverloadedStrings #-}

module Pipeline.RecoverySpec (spec) where

import Data.Maybe (isJust)
import qualified Data.Text as T

import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Betzac.Utils.Unparse (unparse)

import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram)

import Text.Megaparsec (errorBundlePretty)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

{- | A statement that fails during lexing itself (an unrecognized character) —
it contributes no tokens to the stream at all, so any recovery it triggers
has to happen inside 'Betzac.Lexer.Lexer.runLexer'.
-}
lexCorruptStmt :: String
lexCorruptStmt = "~;\n"

{- | A statement that lexes cleanly but can't parse — a bare ';' with nothing
before it. 'Betzac.Parser.Parser.parseStmt' needs at least one atom/modifier,
so this is a parser-level, not lexer-level, failure.
-}
parseCorruptStmt :: String
parseCorruptStmt = ";\n"

{- | A freshly generated, known-valid program rendered back to source, paired
with its statement count.
-}
genSource :: Gen (String, Int)
genSource = do
    prog <- genProgram
    pure (unlex (unparse prog), length prog)

hadAnyRecoveredError :: PipelineResult -> Bool
hadAnyRecoveredError pr =
    maybe False (isJust . snd) (lexResult pr) || maybe False (isJust . snd) (parseResult pr)

spec :: Spec
spec = describe "Pipeline recovery" $ do
    describe "splicing one corrupted statement between two valid ones" $
        it "still parses both valid neighbors and records an error, whether the corruption is a lexer or a parser failure" $
            hedgehog $ do
                (srcA, nA) <- forAll genSource
                (srcB, nB) <- forAll genSource
                corrupt <- forAll $ Gen.element [lexCorruptStmt, parseCorruptStmt]
                let src = T.pack $ srcA <> "\n" <> corrupt <> srcB
                pr <- evalEither (fromScratch "<test>" src)
                (prog, _) <- evalMaybe (parseResult pr)
                length prog === nA + nB
                assert $ hadAnyRecoveredError pr
                assert $ isJust (semanticResult pr)

    describe "termination and cross-stage propagation" $ do
        it "recovers from a statement with no terminating ';' before eof, instead of looping forever" $
            case fromScratch "<test>" "fW" of
                Left e -> expectationFailure (show e)
                Right pr -> do
                    fmap fst (parseResult pr) `shouldBe` Just []
                    fmap (isJust . snd) (parseResult pr) `shouldBe` Just True

        it "still runs the parser and semantic stage over an empty best-effort token stream when lexing found nothing usable" $
            case fromScratch "<test>" "~~~~~" of
                Left e -> expectationFailure (show e)
                Right pr -> do
                    fmap fst (lexResult pr) `shouldBe` Just []
                    fmap (isJust . snd) (lexResult pr) `shouldBe` Just True
                    fmap fst (parseResult pr) `shouldBe` Just []
                    semanticResult pr `shouldSatisfy` isJust

        it "repairs a statement with a lexer-corrupted character instead of dropping it, leaving its own ';' for the parser" $
            case fromScratch "<test>" "A;\nX~;\nB;\n" of
                Left e -> expectationFailure (show e)
                Right pr -> do
                    -- "X~;" recovers to the valid statement "X;" once the bad
                    -- character is skipped without swallowing its own ';' — so
                    -- all three statements (A;, X;, B;) survive.
                    fmap (length . fst) (parseResult pr) `shouldBe` Just 3
                    fmap (isJust . snd) (lexResult pr) `shouldBe` Just True

        it "recovers from several consecutive corrupted statements in a row" $
            case fromScratch "<test>" "~;\n~;\nA;\n" of
                Left e -> expectationFailure (show e)
                Right pr -> do
                    fmap (length . fst) (parseResult pr) `shouldBe` Just 1
                    fmap (isJust . snd) (lexResult pr) `shouldBe` Just True

        it "leaves a clean file with no recovered errors and a semantic pass that runs" $
            case fromScratch "<test>" "X = fW;\n" of
                Left e -> expectationFailure (show e)
                Right pr -> do
                    hadAnyRecoveredError pr `shouldBe` False
                    semanticResult pr `shouldSatisfy` isJust

        it "reports a malformed assignment's own error, not a misleading \"expecting ';'\" from backtracking into a bare label reference" $
            case fromScratch "<test>" ":hello: = f;\n" of
                Left e -> expectationFailure (show e)
                Right pr -> case parseResult pr of
                    Just (_, Just bundle) -> errorBundlePretty bundle `shouldContain` "atom"
                    _ -> expectationFailure "expected a recorded parse error"
