module Lexer.LexerQC (spec, unlex) where

import Arbitrary ()
import Betzac.Alphabet.Comp (compAlphabet)
import Betzac.Alphabet.Expr (whitespace)
import Betzac.Lexer.Lexer (lexSource, runLexer, lexTokens)
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
    TokEndStmt -> ";"
    TokUsing p -> " using " <> p <> " "
    TokOverride -> " override "
    TokExport -> " export "

unlex :: [Token] -> String
unlex = intercalate " " . map unlexOne

lexableExpr :: Gen String
lexableExpr = unlex <$> listOf1 arbitrary

prop_lexableNoLeadingWhitespace :: Property
prop_lexableNoLeadingWhitespace = forAll lexableExpr noLeadingWhitespace
  where
    noLeadingWhitespace [] = True
    noLeadingWhitespace (c : _) = c `notElem` whitespace

badChar :: Gen Char
badChar = arbitrary `suchThat` \c -> c `notElem` compAlphabet <> whitespace

semiLexableExpr :: Gen String
semiLexableExpr = (<>) <$> lexableExpr <*> listOf1 badChar

-- manyLexableStatements :: Gen String
-- manyLexableStatements = sized $ \n -> let n' = round $ sqrt (fromIntegral n :: Double) in intercalate ";" <$> vectorOf n' (resize n' lexable)

prop_lexableNeverFails :: Property
prop_lexableNeverFails = forAll lexableExpr $ \s ->
    case runLexer lexSource s of
        Left _ -> False
        Right _ -> True

-- No more tokens than characters
prop_informationReduction :: Property
prop_informationReduction = forAll lexableExpr $ \s -> case runLexer lexSource s of
    Left _ -> False
    Right (out, _, _) -> length (lexTokens out) <= length s

prop_failOnGarbage :: Property
prop_failOnGarbage = forAll semiLexableExpr $ \s ->
    case runLexer lexSource s of
        Left _ -> True
        Right _ -> False

prop_roundTrip :: Property
prop_roundTrip = forAll (listOf1 arbitrary) $ \tokens ->
    case runLexer lexSource (unlex tokens) of
        Left _ -> False
        Right (out, _, _) -> (lexTokens out) == tokens

spec :: Spec
spec = describe "Lexer.Core" $ do
    context "test generators" $ do
        prop "lexable input is considered not to have leading whitespace" prop_lexableNoLeadingWhitespace
    describe "lexer" $ do
        prop "never fails on lexable strings" prop_lexableNeverFails
        prop "reduces the amount of information of lexable input" prop_informationReduction
        prop "fails on garbage characters" prop_failOnGarbage
        prop "lex . unlex = id" prop_roundTrip
