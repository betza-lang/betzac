module Lexer.LexerQC (spec, unlex) where

import Arbitrary ()
import Betzac.Alphabet.Comp (compAlphabet)
import Betzac.Alphabet.Expr (whitespace)
import Betzac.Lexer.Lexer (runLexer)
import Betzac.Located (tokenVal)
import Betzac.Token
import Data.List (intercalate)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

unlexOne :: Token -> String
unlexOne t = case t of
    TokAtom c -> [c]
    TokDescriptor d -> ":" <> d <> ":"
    TokDirection c -> [c]
    TokBehaviour c -> [c]
    TokLParen -> "("
    TokRParen -> ")"
    TokLBracket -> "["
    TokRBracket -> "]"
    TokLBrace -> "{"
    TokRBrace -> "}"
    TokLAngle -> "<"
    TokRAngle -> ">"
    TokChainStep -> "-"
    TokChainSequence -> "--"
    TokBang -> "!"
    TokSlippery -> "0*"
    TokNumber n -> show n
    TokComma -> ","
    TokAssign -> "="
    TokEndStmt -> ";\n"
    TokUsing p -> " using " <> p <> " "
    TokOverride -> " override "
    TokExport -> " export "

unlex :: [Token] -> String
unlex = intercalate " " . map unlexOne

badChar :: Gen Char
badChar = arbitrary `suchThat` \c -> c `notElem` compAlphabet <> whitespace

semiLexableExpr :: Gen String
semiLexableExpr = (<>) <$> (unlex <$> listOf1 arbitrary) <*> listOf1 badChar

prop_lexableNeverFails :: Property
prop_lexableNeverFails = forAll (unlex <$> listOf1 arbitrary) $ \s ->
    case runLexer "<test>" s of
        Left _ -> False
        Right _ -> True

prop_failOnGarbage :: Property
prop_failOnGarbage = forAll semiLexableExpr $ \s ->
    case runLexer "<test>" s of
        Left _ -> True
        Right _ -> False

prop_roundTrip :: Property
prop_roundTrip = forAll (listOf1 arbitrary) $ \tokens ->
    case runLexer "<test>" (unlex tokens) of
        Left _ -> False
        Right ts -> (tokenVal <$> ts) == tokens

spec :: Spec
spec = describe "Lexer.Core" $ do
    describe "lexer" $ do
        prop "never fails on lexable strings" prop_lexableNeverFails
        prop "fails on garbage characters" prop_failOnGarbage
        prop "lex . unlex = id" prop_roundTrip
