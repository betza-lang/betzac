module Lexer.LexerSpec (spec) where

import Betzac.Alphabet.Expr (behaviour, direction, upper)
import Betzac.Lexer.Lexer (runLexer)
import Betzac.Located (tokenVal)
import Betzac.Token
import Data.Either (isLeft)
import Test.Hspec

tok :: String -> Either String [Token]
tok s = case runLexer "<test>" s of
    (_, Just bundle) -> Left (show bundle)
    (ts, Nothing) -> Right (tokenVal <$> ts)

spec :: Spec
spec = describe "Lexer.Lexer" $ do
    describe "lexToken" $ do
        describe "atoms" $ do
            it "lexes an uppercase letter as an atom" $
                tok "Q" `shouldBe` Right [TokAtom 'Q']
            it "lexes all uppercase letters" $
                mapM_ (\c -> tok [c] `shouldBe` Right [TokAtom c]) upper

        describe "descriptors" $ do
            it "lexes a simple descriptor" $
                tok ":hello:" `shouldBe` Right [TokDescriptor "hello"]
            it "lexes a descriptor with spaces" $
                tok ":my piece:" `shouldBe` Right [TokDescriptor "my piece"]
            it "lexes a leaper descriptor" $
                tok ":3,4:" `shouldBe` Right [TokDescriptor "3,4"]
            it "fails on an unclosed descriptor" $
                tok ":hello" `shouldSatisfy` isLeft

        describe "directions" $ do
            it "lexes a direction modifier" $
                mapM_ (\c -> tok [c] `shouldBe` Right [TokDirection c]) direction

        describe "behaviours" $ do
            it "lexes a behaviour modifier" $
                mapM_ (\c -> tok [c] `shouldBe` Right [TokBehaviour c]) behaviour

        describe "chain operators" $ do
            it "lexes a chain step" $
                tok "-" `shouldBe` Right [TokChainStep]
            it "lexes a chain sequence" $
                tok "--" `shouldBe` Right [TokChainSequence]
            it "prefers chain sequence over chain step" $
                tok "--" `shouldBe` Right [TokChainSequence]

        describe "numbers" $ do
            it "lexes zero" $
                tok "0" `shouldBe` Right [TokNumber 0]
            it "lexes a positive integer" $
                tok "42" `shouldBe` Right [TokNumber 42]
            it "lexes 0* as slippery" $
                tok "0*" `shouldBe` Right [TokSlippery]
            it "does not lex a bare * as slippery" $
                tok "*" `shouldSatisfy` isLeft

        describe "grouping" $ do
            it "lexes parentheses" $
                tok "()" `shouldBe` Right [TokLParen, TokRParen]
            it "lexes brackets" $
                tok "[]" `shouldBe` Right [TokLBracket, TokRBracket]
            it "lexes braces" $
                tok "{}" `shouldBe` Right [TokLBrace, TokRBrace]
            it "lexes angles" $
                tok "<>" `shouldBe` Right [TokLAngle, TokRAngle]

    describe "directives" $ do
        it "lexes export directive" $
            tok "export fW" `shouldBe` Right [TokExport, TokDirection 'f', TokAtom 'W']
        it "lexes override directive" $
            tok "override fW" `shouldBe` Right [TokOverride, TokDirection 'f', TokAtom 'W']
        it "lexes using directive with simple path" $
            tok "using Std" `shouldBe` Right [TokUsing "Std"]
        it "lexes using directive with dotted path" $
            tok "using Std.Prelude" `shouldBe` Right [TokUsing "Std.Prelude"]
        it "fails on directive keyword without following whitespace" $
            tok "exportfW" `shouldSatisfy` isLeft
        it "fails on override without following whitespace" $
            tok "overridefW" `shouldSatisfy` isLeft

    describe "lexSource" $ do
        it "lexes an empty file" $
            tok "" `shouldBe` Right []
        it "lexes a file with only whitespace" $
            tok "   \n\t  " `shouldBe` Right []
        it "lexes a file with only a comment" $
            tok "# a comment\n" `shouldBe` Right []
        it "lexes a simple statement" $
            tok "fW;" `shouldBe` Right [TokDirection 'f', TokAtom 'W', TokEndStmt]
        it "lexes multiple statements" $
            tok "fW;\nA;"
                `shouldBe` Right
                    [TokDirection 'f', TokAtom 'W', TokEndStmt, TokAtom 'A', TokEndStmt]
        it "lexes a label assignment" $
            tok "Q = fW;"
                `shouldBe` Right
                    [TokAtom 'Q', TokAssign, TokDirection 'f', TokAtom 'W', TokEndStmt]
        it "lexes a descriptor assignment" $
            tok ":queen: = fW;"
                `shouldBe` Right
                    [TokDescriptor "queen", TokAssign, TokDirection 'f', TokAtom 'W', TokEndStmt]
        it "fails on garbage characters" $
            tok "fW~;" `shouldSatisfy` isLeft
        it "ignores comments between tokens" $
            tok "f # direction\nW # atom\n;"
                `shouldBe` Right
                    [TokDirection 'f', TokAtom 'W', TokEndStmt]
