{-# LANGUAGE OverloadedStrings #-}

module Debug.DotSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T

import Betzac.Debug.Dot.Visitor (toDot)
import Betzac.Utils.Unparse (unparse)

import AST.DesugarSpec (desugared, parsed)
import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram)

import Hedgehog
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog

-- | The colours the renderer reserves for what the source did not say.
impliedColour, restatedColour, contradictedColour :: Text
impliedColour = "#dcc6f5"
restatedColour = "#ffd9a0"
contradictedColour = "#f2a0a0"

tinted :: Text -> Text -> Int
tinted colour = T.count colour

psGraph, dsGraph :: Text -> Text
psGraph = toDot . parsed
dsGraph = toDot . desugared

spec :: Spec
spec = describe "dot" $ do
    describe "a parsed tree" $ do
        it "has nothing the source did not say" $
            tinted impliedColour (psGraph "export X = W - K;") `shouldBe` 0
        it "cannot restate anything either, no default having been supplied yet" $
            tinted restatedColour (psGraph "export X = acmW;") `shouldBe` 0

    describe "a desugared tree" $ do
        it "draws the direction, continuation and final-leg cm it supplied" $
            tinted impliedColour (dsGraph "export X = W - K;") `shouldBe` 10
        it "draws more nodes than the tree it came from" $
            T.count "label=" (dsGraph "export X = W - K;")
                `shouldSatisfy` (> T.count "label=" (psGraph "export X = W - K;"))
        it "draws an exponent's repetition count, not merely that it repeats" $
            T.count "label=\"3\"" (dsGraph "export X = fsW3;") `shouldBe` 1
        it "draws a contradicted modifier in a colour of its own" $
            tinted contradictedColour (dsGraph "export X = m(caW);") `shouldBe` 2
        it "marks a restatement where it stands" $
            tinted restatedColour (dsGraph "export X = acmW;") `shouldBe` 3
        it "leaves a restatement's children written, the letter having been typed" $
            T.count "\"All\" fillcolor=\"#ccf0ff\"" (dsGraph "export X = acmW;") `shouldBe` 1

    describe "over any program" $
        it "never tints a parsed tree, every node of which was written" $
            hedgehog $ do
                prog <- forAll genProgram
                let source = T.pack . unlex . unparse $ prog
                    graph = psGraph source
                tinted impliedColour graph + tinted restatedColour graph === 0
