module Compilation.InterfaceSpec (spec) where

import Data.Either (isLeft)
import Data.List (sortOn)
import qualified Data.Text as T

import Betzac.Compilation.Interface (
    DependencyStamp (..),
    Interface (..),
    InterfaceEntry,
    hashText,
    ieLabel,
    ieOrigin,
    ieSpan,
    interfaceEntry,
    interfaceHash,
    parseInterface,
    renderInterface,
 )
import Betzac.Span (Span (..))
import Text.Megaparsec (SourcePos (..), mkPos)

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog

-- | Spaces included: a path may hold one.
genPath :: Gen FilePath
genPath = Gen.string (Range.linear 1 20) (Gen.element "/abc .-_")

-- | The descriptor alphabet, which is alphanumerics, comma and space.
genLabel :: Gen String
genLabel = Gen.string (Range.linear 1 12) (Gen.choice [Gen.alphaNum, Gen.element ", "])

genSpan :: Gen Span
genSpan = Gen.choice [pure Generated, RealSpan <$> genPos <*> genPos]
  where
    genPos = SourcePos "" <$> genLine <*> genLine
    genLine = mkPos <$> Gen.int (Range.linear 1 5000)

genEntry :: Gen InterfaceEntry
genEntry =
    interfaceEntry
        <$> genLabel
        <*> Gen.int (Range.linear 0 500)
        <*> genPath
        <*> genSpan

genInterface :: Gen Interface
genInterface =
    Interface
        <$> genHash
        <*> Gen.list (Range.linear 0 4) (DependencyStamp <$> genPath <*> genHash)
        <*> Gen.list (Range.linear 0 6) genEntry
  where
    genHash = hashText . T.pack <$> genPath

-- | What rendering settles: exports come back in label order.
rendered :: Interface -> Interface
rendered i = i{ifExports = sortOn ieLabel (ifExports i)}

spec :: Spec
spec = describe "Compilation.Interface" $ do
    describe "parseInterface" $ do
        it "rejects a file that is not an interface at all" $
            parseInterface (T.pack "W = :1,0:;\n") `shouldSatisfy` isLeft

        it "rejects an interface written by another format version" $
            parseInterface (T.replace (T.pack "-bi 1") (T.pack "-bi 99") (renderInterface emptyInterface))
                `shouldSatisfy` isLeft

        it "rejects a truncated export rather than dropping the label it names" $
            parseInterface (T.pack "betzac-bi 1\nsource 0000000000000000\nexport 0 1 1\n")
                `shouldSatisfy` isLeft

        it "rejects an export naming a file the table does not hold" $
            parseInterface (T.pack "betzac-bi 1\nsource 0000000000000000\nexport 0 0 0 0 0 7 X\n")
                `shouldSatisfy` isLeft

    describe "renderInterface" $
        it "interns each origin once, however many exports name it" $ do
            let entry n = interfaceEntry n 0 "/a b/base.betza" Generated
                i = Interface (hashText $ T.pack "") [] [entry "one two", entry "three"]
            length (filter (T.isPrefixOf $ T.pack "file ") (T.lines $ renderInterface i)) `shouldBe` 1

    describe "interfaceEntry" $
        it "anchors a span to the file that defines the label, not the one re-exporting it" $ do
            let midSpan = RealSpan (SourcePos "/mid.betza" (mkPos 2) (mkPos 1)) (SourcePos "/mid.betza" (mkPos 2) (mkPos 9))
                entry = interfaceEntry "X" 0 "/base.betza" midSpan
            ieOrigin entry `shouldBe` "/base.betza"
            ieSpan entry `shouldBe` RealSpan (SourcePos "/base.betza" (mkPos 2) (mkPos 1)) (SourcePos "/base.betza" (mkPos 2) (mkPos 9))

    describe "properties" $ do
        it "round-trips through its rendering" $ hedgehog $ do
            i <- forAll genInterface
            parseInterface (renderInterface i) === Right (rendered i)

        it "hashes over the exports alone, blind to the source and the dependencies" $ hedgehog $ do
            i <- forAll genInterface
            j <- forAll genInterface
            interfaceHash i === interfaceHash i{ifSource = ifSource j, ifDeps = ifDeps j}

        it "hashes differently once an export is added" $ hedgehog $ do
            i <- forAll genInterface
            e <- forAll genEntry
            interfaceHash i{ifExports = e : ifExports i} /== interfaceHash i
  where
    emptyInterface = Interface (hashText $ T.pack "") [] []
