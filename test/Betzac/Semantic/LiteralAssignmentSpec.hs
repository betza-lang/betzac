{-# LANGUAGE OverloadedStrings #-}

module Semantic.LiteralAssignmentSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Types (BetzaStmt (Assign), Label (Leaper))
import Betzac.AST.Utils (stmtOf)
import Betzac.Diagnostic (
    SemanticProblem (semKind, semSpan),
    SemanticProblemKind (InvalidStatement),
    causeOf,
 )
import Betzac.Pipeline (PipelineResult (parseResult, semanticResult), fromScratch)
import Betzac.Span (Span (..))

import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram, unparse)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog
import Text.Megaparsec.Pos (SourcePos (sourceLine), unPos)

{- | Every problem the single-file semantic passes report for a snippet. Fails loudly
on a snippet that doesn't parse: a fixture that never reaches the passes would report
nothing and pass every expectation below for the wrong reason.
-}
problemsOf :: Text -> [SemanticProblem]
problemsOf src = case fromScratch "<test>" src of
    Left e -> error ("pipeline error: " ++ show e)
    Right pr -> case parseResult pr of
        Just (_, Just bundle) -> error ("parse error: " ++ show bundle)
        Just (_, Nothing) -> maybe (error "semantic passes did not run") id (semanticResult pr)
        Nothing -> error "parser did not run"

causes :: Text -> [String]
causes = map (causeOf . semKind) . problemsOf

invalidStatement :: String
invalidStatement = causeOf (InvalidStatement "")

-- | The lines a snippet's invalid-statement problems are reported on.
reportedLines :: Text -> [Int]
reportedLines src =
    [ lineOf p
    | p <- problemsOf src
    , causeOf (semKind p) == invalidStatement
    ]
  where
    lineOf p = case semSpan p of
        RealSpan s _ -> unPos (sourceLine s)
        Generated -> 0

-- | Leaper assignments as source text, mixed into generated programs.
offenders :: [String]
offenders = [":1,1: = fW;", ":2,1: = fF;", ":3,2: = fW fF;"]

spec :: Spec
spec = describe "Semantic.LiteralAssignment" $ do
    describe "assigning to a leaper literal" $ do
        it "reports the statement, and nothing else" $
            causes ":1,1: = fW;\n" `shouldBe` [invalidStatement]

        it "reports it through an export directive" $
            causes "export :2,1: = fW;\n" `shouldBe` [invalidStatement]

        it "reports it through an override directive" $
            causes "override :2,1: = fW;\n" `shouldBe` [invalidStatement]

        it "reports it through an overridden export" $
            causes "override export :2,1: = fW;\n" `shouldBe` [invalidStatement]

        it "reports each offending statement on its own line, once" $
            reportedLines ":1,1: = fW;\n:2,1: = fF;\n" `shouldBe` [1, 2]

    describe "what stays legal" $ do
        it "leaves a leaper used as a bare reference alone" $
            causes ":1,1:;\n" `shouldBe` []

        it "leaves a leaper on the right-hand side alone" $
            causes "A = :1,1:;\n" `shouldBe` []

        it "leaves an assignment to a descriptor label alone" $
            causes ":cat sword: = fW;\n" `shouldBe` []

        it "leaves an assignment to an uppercase label alone" $
            causes "A = fW;\n" `shouldBe` []

    describe "property" $
        it "reports exactly one invalid statement per leaper assignment, whatever else the program contains" $
            hedgehog $ do
                prog <- forAll genProgram
                injected <- forAll $ Gen.list (Range.linear 0 3) (Gen.element offenders)
                let src = T.pack (unlex (unparse prog) ++ "\n" ++ unlines injected)
                    generated =
                        length
                            [ ()
                            | qs <- prog
                            , Just (Assign (Leaper _ _ _) _ _) <- [stmtOf qs]
                            ]
                    expected = generated + length injected
                    reported = length [() | c <- causes src, c == invalidStatement]
                annotate (T.unpack src)
                cover 10 "offenders from the generator itself" (generated > 0)
                cover 15 "several offenders at once" (expected > 1)
                cover 20 "no offender at all" (expected == 0)
                reported === expected
