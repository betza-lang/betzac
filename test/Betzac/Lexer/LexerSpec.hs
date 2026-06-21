module Lexer.LexerSpec (spec) where

import Betzac.Alphabet.Expr (behaviour, direction, upper)
import Betzac.Lexer.Core (LexError, runLexer)
import Betzac.Lexer.Lexer (lexSource, lexToken, lexTokens)
import Betzac.Token
import Data.Either (isLeft)
import Test.Hspec

tok :: String -> Either LexError [Token]
tok s = case runLexer lexSource s of
    Left e -> Left e
    Right (out, _, _) -> Right (lexTokens out)

oneTok :: String -> Either LexError Token
oneTok s = case runLexer lexToken s of
    Left e -> Left e
    Right (t, _, _) -> Right t

spec :: Spec
spec = describe "Lexer.Lexer" $ do
    describe "lexToken" $ do
        describe "atoms" $ do
            it "lexes an uppercase letter as an atom" $
                oneTok "Q" `shouldBe` Right (TokAtom 'Q')
            it "lexes all uppercase letters" $
                mapM_ (\c -> oneTok [c] `shouldBe` Right (TokAtom c)) upper

        describe "descriptors" $ do
            it "lexes a simple descriptor" $
                oneTok ":hello:" `shouldBe` Right (TokDescriptor "hello")
            it "lexes a descriptor with spaces" $
                oneTok ":my piece:;" `shouldBe` Right (TokDescriptor "my piece")
            it "lexes a leaper descriptor" $
                oneTok ":3,4:;" `shouldBe` Right (TokDescriptor "3,4")
            it "fails on an unclosed descriptor" $
                oneTok ":hello" `shouldSatisfy` isLeft

        describe "directions" $ do
            it "lexes a direction modifier" $
                mapM_ (\c -> oneTok [c] `shouldBe` Right (TokDirection c)) direction

        describe "behaviours" $ do
            it "lexes a behaviour modifier" $
                mapM_ (\c -> oneTok [c] `shouldBe` Right (TokBehaviour c)) behaviour

        describe "chain operators" $ do
            it "lexes a chain step" $
                oneTok "-" `shouldBe` Right TokChainStep
            it "lexes a chain sequence" $
                oneTok "--" `shouldBe` Right TokChainSequence
            it "prefers chain sequence over chain step" $
                oneTok "--" `shouldBe` Right TokChainSequence

        describe "numbers" $ do
            it "lexes zero" $
                oneTok "0" `shouldBe` Right (TokNumber 0)
            it "lexes a positive integer" $
                oneTok "42" `shouldBe` Right (TokNumber 42)
            it "lexes 0* as slippery" $
                oneTok "0*" `shouldBe` Right TokSlippery
            it "does not lex a bare * as slippery" $
                oneTok "*" `shouldSatisfy` isLeft

        describe "grouping" $ do
            it "lexes parentheses" $
                tok "();" `shouldBe` Right [TokLParen, TokRParen, TokEndStmt]
            it "lexes brackets" $
                tok "[];" `shouldBe` Right [TokLBracket, TokRBracket, TokEndStmt]
            it "lexes braces" $
                tok "{};" `shouldBe` Right [TokLBrace, TokRBrace, TokEndStmt]
            it "lexes angles" $
                tok "<>;" `shouldBe` Right [TokLAngle, TokRAngle, TokEndStmt]

    describe "directives" $ do
        it "lexes export directive" $
            tok "export fW;" `shouldBe` Right [TokExport, TokDirection 'f', TokAtom 'W', TokEndStmt]
        it "lexes override directive" $
            tok "override fW;" `shouldBe` Right [TokOverride, TokDirection 'f', TokAtom 'W', TokEndStmt]
        it "lexes using directive with simple path" $
            tok "using Std;" `shouldBe` Right [TokUsing "Std", TokEndStmt]
        it "lexes using directive with dotted path" $
            tok "using Std.Prelude;" `shouldBe` Right [TokUsing "Std.Prelude", TokEndStmt]
        it "fails on directive keyword without following whitespace" $
            tok "exportfW;" `shouldSatisfy` isLeft
        it "fails on override without following whitespace" $
            tok "overridefW;" `shouldSatisfy` isLeft

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
