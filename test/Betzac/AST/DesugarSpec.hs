{-# LANGUAGE OverloadedStrings #-}

module AST.DesugarSpec (spec, parsed, desugared) where

import Data.Text (Text)
import qualified Data.Text as T

import Betzac.AST
import Betzac.AST.Desugar (desugar)
import Betzac.AST.Origin (Origin (..), origin)
import Betzac.Pipeline (PipelineResult (parseResult), fromScratch)
import Betzac.Utils.Unparse (unparse)

import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram)

import Hedgehog
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Hedgehog

{- | The parse of a snippet, erroring loudly rather than letting an unparseable
fixture satisfy every expectation below for the wrong reason.
-}
parsed :: Text -> BetzaProgram Ps
parsed src = case fromScratch "<test>" src of
    Left e -> error ("pipeline error: " ++ show e)
    Right pr -> case parseResult pr of
        Just (_, Just bundle) -> error ("parse error: " ++ show bundle)
        Just (prog, Nothing) -> prog
        Nothing -> error "parser did not run"

desugared :: Text -> BetzaProgram Ds
desugared = desugar . parsed

-- | How many directions a desugaring supplied for itself.
suppliedDirections :: Text -> Int
suppliedDirections = countDirections Implied

-- | How many written directions a desugaring found it was about to supply.
restatedDirections :: Text -> Int
restatedDirections = countDirections Restated

-- | How many written behaviours restate what a final leg carries anyway.
restatedBehaviours :: Text -> Int
restatedBehaviours = countBehaviours Restated

-- | How many behaviours a desugaring supplied for itself.
suppliedBehaviours :: Text -> Int
suppliedBehaviours = countBehaviours Implied

countDirections :: Origin -> Text -> Int
countDirections o src =
    length [() | d <- universeOf (desugared src) :: [DirectionModifier Ds], origin d == o]

countBehaviours :: Origin -> Text -> Int
countBehaviours o src =
    length [() | b <- universeOf (desugared src) :: [Behaviour Ds], origin b == o]

-- | Every behaviour the tree ends up carrying, by kind and modality.
behavioursIn :: Text -> [BehaviourKind Stripped]
behavioursIn src =
    [strip k | Behaviour k _ _ <- universeOf (desugared src) :: [Behaviour Ds]]

-- | Every joint modality the tree ends up carrying, written or supplied.
modalitiesIn :: Text -> [ChainModality Stripped]
modalitiesIn src = map strip (universeOf (desugared src) :: [ChainModality Ds])

spec :: Spec
spec = describe "desugar" $ do
    describe "the direction a leg does not write" $ do
        it "supplies the identity restriction on a leg that inherits nothing" $
            suppliedDirections "export X = W;" `shouldBe` 1
        it "leaves a written direction alone" $
            suppliedDirections "export X = fW;" `shouldBe` 0
        it "supplies f on a step continuation, and a on the head it follows" $
            suppliedDirections "export X = cmW - K;" `shouldBe` 2
        it "supplies the identity restriction after a sequence chain, which inherits nothing" $
            suppliedDirections "export X = cmW -- K;" `shouldBe` 2

    describe "the cm a final leg carries anyway" $ do
        it "supplies both halves on an unconditionally final leg" $
            suppliedBehaviours "export X = aW;" `shouldBe` 2
        it "supplies neither before a mandatory continuation" $
            suppliedBehaviours "export X = (aK - acmY);" `shouldBe` 0
        it "supplies neither where the leg writes one half itself" $
            suppliedBehaviours "export X = acW;" `shouldBe` 0
        it "supplies neither under an exponent, every copy carrying what the head writes" $
            suppliedBehaviours "export X = aW0;" `shouldBe` 0

    describe "what the language would have supplied anyway" $ do
        it "flags a written a on a leg whose direction is not inherited" $
            restatedDirections "export X = acW;" `shouldBe` 1
        it "flags a written a after a sequence chain, which inherits nothing" $
            restatedDirections "export X = cmW -- aK;" `shouldBe` 1
        it "flags a written f on a leg that inherits its direction" $
            restatedDirections "export X = cmW - cmfK;" `shouldBe` 1
        it "flags cm on an unconditionally final leg" $
            restatedBehaviours "export X = acmW;" `shouldBe` 2
        it "stays silent on cm before a mandatory continuation" $
            restatedBehaviours "export X = (acmK - abK);" `shouldBe` 0
        it "stays silent on cm before a continuation that may be declined" $
            restatedBehaviours "export X = (acmK - [aQ]);" `shouldBe` 0
        it "stays silent on a that cancels the f of a step continuation" $
            restatedDirections "export X = cmW - (aK - Y);" `shouldBe` 0
        it "stays silent on c alone, which narrows rather than restates" $
            restatedBehaviours "export X = acW;" `shouldBe` 0

    describe "a behaviour written on a parenthesised chain" $ do
        it "narrows the final leg's cm rather than widening it" $
            behavioursIn "export X = m(aR);" `shouldBe` [Move ()]
        it "reaches the leg that ends the chain, not the one that starts it" $
            behavioursIn "export X = c(aR - aR);" `shouldBe` [Capture ()]
        it "leaves the same leg qualified as writing it there would have" $
            behavioursIn "export X = m(aR);" `shouldBe` behavioursIn "export X = maR;"
        it "permits nothing where it contradicts what the leg states" $
            behavioursIn "export X = m(caR);" `shouldBe` []
        it "restates the final leg's default when it supplies both halves" $
            restatedBehaviours "export X = cm(aR);" `shouldBe` 2
        it "supplies no default of its own, having been told what the leg permits" $
            suppliedBehaviours "export X = m(aR);" `shouldBe` 0

    describe "an exponent's joints" $ do
        it "supplies the optional modality where none is written" $
            modalitiesIn "export X = aW0;" `shouldBe` [Choose ()]
        it "keeps a written modality" $
            modalitiesIn "export X = aW-3;" `shouldBe` [Mandatory ()]
        it "reads 0* as the braced modality it means" $
            modalitiesIn "export X = aW0*;" `shouldBe` [IffUnblocked ()]
        it "leaves 0* and -{0} indistinguishable, being two spellings of one construct" $
            map strip (desugared "export X = aW0*;")
                `shouldBe` map strip (desugared "export X = aW-{0};")

    describe "round trip" $
        it "is idempotent through its own rendering" $
            hedgehog $ do
                prog <- forAll genProgram
                let source = T.pack . unlex . unparse $ prog
                    once = desugared source
                    twice = desugar (parsed (T.pack . unlex . unparse $ once))
                map strip twice === map strip once
