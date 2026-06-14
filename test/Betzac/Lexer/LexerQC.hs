module Lexer.LexerQC (spec) where

import Betzac.Alphabet.Comp (compAlphabet)
import Betzac.Alphabet.Expr (alphanum, behaviour, direction, space, upper, whitespace)
import Betzac.Lexer.Lexer (lexSource, runLexer)
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

descriptor :: Gen String
descriptor = intercalate [space] <$> resize 4 (listOf1 $ resize 7 $ listOf1 $ elements (',' : alphanum))

exprToken :: Gen Token
exprToken = sized $ \n ->
    oneof
        [ TokAtom <$> elements upper
        , TokDescriptor <$> descriptor
        , TokDirection <$> elements direction
        , TokBehaviour <$> elements behaviour
        , oneof [pure TokLParen, pure TokRParen]
        , oneof [pure TokLBracket, pure TokRBracket]
        , oneof [pure TokLBrace, pure TokRBrace]
        , oneof [pure TokLAngle, pure TokRAngle]
        , oneof [pure TokChainStep, pure TokChainSequence]
        , pure TokBang
        , pure TokSlippery
        , frequency
            [ (1, pure $ TokNumber 0)
            , (3, TokNumber <$> choose (0, let n' = (n + 1) `div` 5 in 10 ^ n'))
            ]
        , pure TokComma
        ]

lexableExpr :: Gen String
lexableExpr = unlex <$> listOf1 exprToken

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
    Right (toks, _, _) -> length toks <= length s

prop_failOnGarbage :: Property
prop_failOnGarbage = forAll semiLexableExpr $ \s ->
    case runLexer lexSource s of
        Left _ -> True
        Right _ -> False

spec :: Spec
spec = describe "Lexer.Core" $ do
    context "test generators" $ do
        prop "lexable input is considered not to have leading whitespace" prop_lexableNoLeadingWhitespace
    describe "expression lexer" $ do
        prop "never fails on lexable strings" prop_lexableNeverFails
        prop "reduces the amount of information of lexable input" prop_informationReduction
        prop "fails on garbage characters" prop_failOnGarbage
