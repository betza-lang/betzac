{-# LANGUAGE OverloadedStrings #-}

module Semantic.RedundantModifierSpec (spec) where

import Data.List (intercalate, isInfixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST.Generic (universeOf)
import Betzac.AST.Phases (Stripped)
import Betzac.AST.Types (ModifierExpr (modifiers))
import Betzac.Compilation.Flag (
    CompilerFlag (GenerateWarnings, PromoteWarningsToError, SuppressWarnings),
    Wspecifier (Wlang),
    applyOptions,
    optionsFromFlags,
 )
import Betzac.Diagnostic (
    SemanticProblem (semKind, semSpan),
    SemanticProblemKind (RedundantModifier),
    Severity (Error, Warning),
    causeOf,
    mkProblem,
    semSev,
 )
import Betzac.Pipeline (PipelineResult (parseResult, semanticResult), fromScratch)
import Betzac.Span (Span (..))
import Betzac.Utils.Unparse (unparse)

import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog
import Text.Megaparsec.Pos (SourcePos (sourceColumn), unPos)

{- | Every problem the single-file semantic passes report for a snippet. Fails loudly on
a snippet that doesn't parse: a fixture that never reaches the passes would report
nothing and satisfy every "stays silent" expectation for the wrong reason.
-}
problemsOf :: Text -> [SemanticProblem]
problemsOf src = case fromScratch "<test>" src of
    Left e -> error ("pipeline error: " ++ show e)
    Right pr -> case parseResult pr of
        Just (_, Just bundle) -> error ("parse error: " ++ show bundle)
        Just (_, Nothing) -> maybe (error "semantic passes did not run") id (semanticResult pr)
        Nothing -> error "parser did not run"

redundant :: String
redundant = causeOf (RedundantModifier "")

-- | Every problem's cause, so a test can assert nothing *else* was reported either.
causes :: Text -> [String]
causes = map (causeOf . semKind) . problemsOf

-- | The columns redundancy is reported on, identifying which modifier was blamed.
blamed :: Text -> [Int]
blamed src =
    [ col p
    | p <- problemsOf src
    , causeOf (semKind p) == redundant
    ]
  where
    col p = case semSpan p of
        RealSpan s _ -> unPos (sourceColumn s)
        Generated -> 0

-- | How many redundancies of a given flavour a snippet reports.
countOf :: String -> Text -> Int
countOf flavour src =
    length
        [ ()
        | p <- problemsOf src
        , RedundantModifier detail <- [semKind p]
        , flavour `isInfixOf` detail
        ]

duplicates, subsumptions :: Text -> Int
duplicates = countOf "duplicate"
subsumptions = countOf "subsumed"

spec :: Spec
spec = describe "Semantic.RedundantModifier" $ do
    describe "duplicate modifiers (3.5.6.1)" $ do
        it "reports a repeated behaviour modifier" $
            duplicates "export X = mmW;\n" `shouldBe` 1

        it "reports a repeated direction modifier" $
            duplicates "export X = ffW;\n" `shouldBe` 1

        it "reports a repeat separated by another modifier" $
            duplicates "export X = mcmW;\n" `shouldBe` 1

        it "reports two repeats of the same modifier separately" $
            duplicates "export X = mmmW;\n" `shouldBe` 2

        it "blames the later occurrence, not the first" $
            blamed "export X = mmW;\n" `shouldBe` [13]

    describe "what is not a duplicate" $ do
        it "leaves two different behaviours alone" $
            causes "export X = cmW;\n" `shouldBe` []

        it "leaves a doubled modality alone, it being one modifier and not two" $
            causes "export X = ccW;\n" `shouldBe` []

        it "leaves the same modifier on two different legs alone" $
            causes "export X = fW fF;\n" `shouldBe` []

        it "leaves the same modifier on either side of a chain alone" $
            causes "export X = fW - fW;\n" `shouldBe` []

    describe "subsumed direction modifiers (3.5.6.2)" $ do
        it "reports a direction subsumed by a wider one" $
            subsumptions "export X = fvW;\n" `shouldBe` 1

        it "reports a direction subsumed by `a`" $
            subsumptions "export X = laW;\n" `shouldBe` 1

        it "reports a sideways direction subsumed by `s`" $
            subsumptions "export X = lsW;\n" `shouldBe` 1

        it "reports an amalgamated direction subsumed by its greater component" $
            subsumptions "export X = <fr>fW;\n" `shouldBe` 1

        it "reports an amalgamated direction subsumed by its lesser component" $
            subsumptions "export X = <fr>rW;\n" `shouldBe` 1

        it "reports every amalgamated direction a single component covers" $
            subsumptions "export X = <fr><rf>fW;\n" `shouldBe` 2

        it "reports a direction covered only by the union of the others" $
            -- `<fr>` is in neither `<rf>` nor `<ff>` alone, but is in the two together.
            subsumptions "export X = <fr><rf><ff>W;\n" `shouldBe` 1

    describe "what is not subsumed" $ do
        it "leaves two disjoint directions alone" $
            causes "export X = fbW;\n" `shouldBe` []

        it "leaves two disjoint amalgamated directions alone" $
            causes "export X = <fr><rf>W;\n" `shouldBe` []

        it "leaves an amalgamated direction beside an unrelated one alone" $
            causes "export X = <fr>bW;\n" `shouldBe` []

        it "leaves a lone wide modifier alone" $
            causes "export X = sW;\n" `shouldBe` []

        it "leaves a lone `a` alone" $
            causes "export X = aW;\n" `shouldBe` []

        it "leaves a doubled direction beside `s` alone, `s` reaching no axis" $
            -- Pins that `<ff>` selects the forward axis: `s` selects every displacement
            -- with a sideways component, which is everything `<ff>` selects *except*
            -- the axis. Were the axis missing from `<ff>`, `s` would subsume it.
            causes "export X = <ff>sW;\n" `shouldBe` []

    describe "subsumed behaviour modifiers (3.5.6.2)" $ do
        -- `m` separates: `c` followed by `cy` lexes as `cc` and a stray `y`.
        it "reports a behaviour subsumed by the same kind's any-modality form" $
            subsumptions "export X = cmcyW;\n" `shouldBe` 1

        it "reports it with the wider modifier on either side" $
            subsumptions "export X = cymcW;\n" `shouldBe` 1

        it "blames the narrow modifier, not the wide one" $
            blamed "export X = cymcW;\n" `shouldBe` [15]

        it "reports a doubled modality subsumed by the any-modality form" $
            subsumptions "export X = ccmcyW;\n" `shouldBe` 1

        it "reports every occurrence the wide modifier covers" $
            subsumptions "export X = cmcmcyW;\n" `shouldBe` 2

        it "reports leaping and jumping the same way" $
            map subsumptions ["export X = gmgyW;\n", "export X = jmjyW;\n"] `shouldBe` [1, 1]

    describe "what behaviour is not subsumed" $ do
        it "leaves a lone any-modality behaviour alone" $
            causes "export X = cyW;\n" `shouldBe` []

        it "leaves a behaviour of another kind alone" $
            causes "export X = mcyW;\n" `shouldBe` []

        it "leaves a doubled modality alone, twice not covering once" $
            causes "export X = ccmcW;\n" `shouldBe` []

        it "leaves hopping alone, it having no any-modality form" $
            causes "export X = pmppW;\n" `shouldBe` []

        it "counts two any-modality behaviours as a duplicate, not a subsumption" $
            map ($ "export X = cymcyW;\n") [subsumptions, duplicates] `shouldBe` [0, 1]

        it "leaves the same behaviour on two different legs alone" $
            causes "export X = cyW cF;\n" `shouldBe` []

    describe "severity" $
        it "reports redundancy as a warning, never an error" $
            map semSev (problemsOf "export X = mmW;\nexport Y = fvW;\n")
                `shouldBe` [Warning, Warning]

    describe "flag governance" $ do
        let redundancy = mkProblem Warning (RedundantModifier "duplicate") Generated
            under fs = applyOptions (optionsFromFlags fs) [redundancy]

        it "stays silent by default, redundancy being a style warning nobody asked for" $
            length (under []) `shouldBe` 0

        it "appears as a warning under -Wlang" $
            map semSev (under [GenerateWarnings Wlang]) `shouldBe` [Warning]

        it "is promoted by -Werror=lang" $
            map semSev (under [PromoteWarningsToError Wlang]) `shouldBe` [Error]

        it "is silenced by -w even when -Wlang asks for it" $
            length (under [SuppressWarnings, GenerateWarnings Wlang]) `shouldBe` 0

    describe "property" $ do
        it "reports one duplicate per modifier a leg repeats, whatever else the program contains" $
            hedgehog $ do
                prog <- forAll genProgram
                injected <- forAll $ Gen.list (Range.linear 0 3) (Gen.element offenders)
                let src = T.pack (unlex (unparse prog) ++ "\n" ++ unlines (map fst injected))
                    repeated ms = length ms - length (nub ms)
                    generated =
                        sum
                            [ repeated (modifiers m)
                            | m <- universeOf prog :: [ModifierExpr Stripped]
                            ]
                    expected = generated + sum (map snd injected)
                annotate (T.unpack src)
                cover 20 "offenders injected" (not (null injected))
                cover 20 "several at once" (expected > 1)
                cover 15 "none at all" (expected == 0)
                duplicates src === expected

        it "reports one subsumption per behaviour another behaviour on the leg covers" $
            hedgehog $ do
                bs <- forAll $ Gen.list (Range.linear 2 6) (Gen.element behaviours)
                let src = T.pack ("export X = " ++ intercalate "m" bs ++ "W;\n")
                    anyModality b = b `elem` ["cy", "gy"]
                    covered b =
                        not (anyModality b)
                            && any (\w -> anyModality w && take 1 w == take 1 b) bs
                    expected = length (filter covered bs)
                annotate (T.unpack src)
                cover 20 "something subsumed" (expected > 0)
                cover 20 "nothing subsumed" (expected == 0)
                subsumptions src === expected

{- | Statements whose legs repeat a modifier, with the number of repeats each contains,
mixed into generated programs. Adjacent repeats only -- the non-adjacent case is pinned
by its own unit test above.
-}
offenders :: [(String, Int)]
offenders =
    [ ("export Q = mmW;", 1)
    , ("export Q = iiW;", 1)
    , ("export Q = mmmW;", 2)
    , ("export Q = ffW;", 1)
    ]

-- | Behaviour modifiers, spelt so that any two of them may sit side by side.
behaviours :: [String]
behaviours = ["c", "cc", "cy", "g", "gy"]
