module Lexer.ScanSpec (spec) where

import Betzac.Lexer.Core (runLexer)
import Betzac.Lexer.Scan
import Data.Either (isLeft)
import Test.Hspec

spec :: Spec
spec = describe "Lexer.Scan" $ do
    describe "lexWhitespace" $ do
        it "consumes a single space" $
            runLexer lexWhitespace " abc" `shouldBe` Right ((), 1, "abc")
        it "consumes multiple whitespace characters" $
            runLexer lexWhitespace "  \t\n abc" `shouldBe` Right ((), 5, "abc")
        it "fails on empty input" $
            runLexer lexWhitespace "" `shouldSatisfy` isLeft
        it "fails on non-whitespace input" $
            runLexer lexWhitespace "abc" `shouldSatisfy` isLeft
        it "does not consume trailing non-whitespace" $
            runLexer lexWhitespace " \t abc" `shouldBe` Right ((), 3, "abc")

    describe "lexComment" $ do
        it "consumes a comment to end of line" $
            runLexer lexComment "# hello world\nabc" `shouldBe` Right ((), 13, "\nabc")
        it "consumes an empty comment" $
            runLexer lexComment "#\nabc" `shouldBe` Right ((), 1, "\nabc")
        it "consumes a comment at end of input" $
            runLexer lexComment "# hello" `shouldBe` Right ((), 7, "")
        it "fails on non-comment input" $
            runLexer lexComment "abc" `shouldSatisfy` isLeft
        it "does not consume the newline terminating the comment" $
            runLexer lexComment "# comment\n" `shouldBe` Right ((), 9, "\n")

    describe "lexIgnore" $ do
        it "succeeds consuming nothing on empty input" $
            runLexer lexIgnore "" `shouldBe` Right ((), 0, "")
        it "succeeds consuming nothing on non-whitespace non-comment input" $
            runLexer lexIgnore "abc" `shouldBe` Right ((), 0, "abc")
        it "consumes leading whitespace" $
            runLexer lexIgnore "   abc" `shouldBe` Right ((), 3, "abc")
        it "consumes a leading comment" $
            runLexer lexIgnore "# comment\nabc" `shouldBe` Right ((), 10, "abc")
        it "consumes interleaved whitespace and comments" $
            runLexer lexIgnore "  # comment\n  # another\nabc" `shouldBe` Right ((), 24, "abc")

    describe "lexIgnoreSomeLeadingWhitespace" $ do
        it "consumes at least one whitespace character" $
            runLexer lexIgnoreSome " abc" `shouldBe` Right ((), 1, "abc")
        it "fails on empty input" $
            runLexer lexIgnoreSome "" `shouldSatisfy` isLeft
        it "fails on non-whitespace input" $
            runLexer lexIgnoreSome "abc" `shouldSatisfy` isLeft
        it "consumes whitespace followed by a comment" $
            runLexer lexIgnoreSome " # comment\nabc" `shouldBe` Right ((), 11, "abc")
