{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Parser.ParserHedgehog (spec, genProgram) where

import Betzac.AST as B
import Betzac.Located
import Betzac.Parser.BetzaTokenStream
import Betzac.Parser.Parser
import qualified Betzac.Token as B
import Betzac.Utils.Unparse

import Lexer.LexerQC (unlex)

import Text.Megaparsec

import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Monoid as M (Any (..), getAny)

import Data.List.NonEmpty (toList)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog

-- generators

genVoid :: Gen ()
genVoid = Gen.constant ()

genDirection :: Gen (Direction Stripped)
genDirection =
    Gen.element
        [ Forward ()
        , Backward ()
        , Leftward ()
        , Rightward ()
        , Sideway ()
        , Vertically ()
        , All ()
        ]

genBehaviour :: Gen (Behaviour Stripped)
genBehaviour =
    Gen.choice
        [ Gen.element
            [ Behaviour (Initial ()) (Once ()) ()
            , Behaviour (Move ()) (Once ()) ()
            , Behaviour (NoJump ()) (Once ()) ()
            ]
        , Behaviour (Capture ()) <$> Gen.element [Once (), Twice (), Any ()] <*> genVoid
        , Behaviour (Leap ()) <$> Gen.element [Once (), Twice (), Any ()] <*> genVoid
        , Behaviour (Jump ()) <$> Gen.element [Once (), Twice (), Any ()] <*> genVoid
        , Behaviour (Hop ()) <$> Gen.element [Once (), Twice (), Any ()] <*> genVoid
        ]

genChainOperator :: Gen (ChainOperator Stripped)
genChainOperator = ChainOperator <$> genChainKind <*> genChainModality <*> genVoid

genChainKind :: Gen (ChainKind Stripped)
genChainKind = Gen.element [Step (), Sequence ()]

genChainModality :: Gen (ChainModality Stripped)
genChainModality = Gen.element [Mandatory (), Choose (), IffUnblocked ()]

genNumber :: Gen Number
genNumber = Gen.sized $ \(Size n) -> Gen.int $ Range.linear 1 (max 1 n)

genExponentKind :: Gen (ExponentKind Stripped)
genExponentKind =
    Gen.choice
        [ Infinite <$> genVoid
        , Slippery <$> genVoid
        , Repeat <$> genNumber <*> genVoid
        ]

genUpper :: Gen (Label Stripped)
genUpper = Upper <$> Gen.element ['A' .. 'Z'] <*> genVoid

genDescriptor :: Gen (Label Stripped)
genDescriptor =
    Descriptor
        <$> do
            wrds <-
                Gen.nonEmpty (Range.linear 1 5) $
                    Gen.string (Range.linear 1 8) Gen.alphaNum
            pure $ intercalate " " (toList wrds)
        <*> genVoid

genLeaper :: Gen (Label Stripped)
genLeaper = Leaper <$> genNumber <*> genNumber <*> genVoid

genLabel :: Gen (Label Stripped)
genLabel = Gen.choice [genUpper, genDescriptor, genLeaper]

genDirectionMod :: Gen (DirectionModifier Stripped)
genDirectionMod =
    Gen.frequency
        [ (2, Single <$> genDirection <*> genVoid)
        , (1, Amalgamated <$> genDirection <*> genDirection <*> genVoid)
        ]

genModifier :: Gen (Modifier Stripped)
genModifier =
    Gen.choice
        [ Directional <$> genDirectionMod <*> genVoid
        , Behavioural <$> genBehaviour <*> genVoid
        ]

genModifiers :: Gen [Modifier Stripped]
genModifiers =
    Gen.filter noAmbiguousSequences $
        Gen.list (Range.linear 0 3) genModifier
  where
    noAmbiguousSequences ms =
        let ts = ms >>= unparseModifier
         in not $ any ambiguous $ zip ts $ drop 1 ts
    ambiguous (B.TokBehaviour a, B.TokBehaviour b) = a == b || b == 'y'
    ambiguous _ = False

genSmallExpr :: Gen (BetzaExpr Stripped)
genSmallExpr = Gen.sized $ \n -> Gen.resize (n `div` 2) genExpr

genAtom :: Gen (AtomExpr Stripped)
genAtom =
    Gen.recursive
        Gen.choice
        [From <$> genLabel <*> genVoid]
        [Paren <$> genSmallExpr <*> genVoid]

genExponent :: Gen (Exponent Stripped)
genExponent =
    Exponent
        <$> Gen.maybe genChainOperator
        <*> genModifiers
        <*> genExponentKind
        <*> genVoid

genExponentExpr :: Gen (ExponentExpr Stripped)
genExponentExpr =
    ExponentExpr
        <$> genAtom
        <*> (Gen.maybe genExponent)
        <*> genVoid

genModifierExpr :: Gen (ModifierExpr Stripped)
genModifierExpr =
    ModifierExpr
        <$> Gen.bool -- setup
        <*> genModifiers
        <*> genExponentExpr
        <*> genVoid

genUnion :: Gen (UnionExpr Stripped)
genUnion = UnionExpr <$> Gen.nonEmpty (Range.linear 1 4) genModifierExpr <*> genVoid

genChain :: Gen (ChainExpr Stripped)
genChain =
    Gen.recursive
        Gen.choice
        [ChainExpr <$> genUnion <*> return Nothing <*> genVoid]
        [ChainExpr <$> genUnion <*> (Just <$> genChainLeg) <*> genVoid]

genChainLeg :: Gen (ChainLeg Stripped)
genChainLeg = ChainLeg <$> genChainOperator <*> genSmallChain <*> genVoid
  where
    genSmallChain = Gen.sized $ \n -> Gen.resize (n `div` 2) genChain

genExpr :: Gen (BetzaExpr Stripped)
genExpr = BetzaExpr <$> genChain <*> genVoid

genStmt :: Gen (BetzaStmt Stripped)
genStmt =
    Gen.choice
        [ LabelRef <$> genLabel <*> genVoid
        , Assign <$> genLabel <*> genExpr <*> genVoid
        ]

genFilePath :: Gen FilePath
genFilePath =
    intercalate "/"
        <$> Gen.list
            (Range.linear 1 10)
            (Gen.string (Range.linear 1 20) Gen.alphaNum)

genDirective :: Gen (Directive Stripped)
genDirective =
    Gen.choice
        [ Bare <$> genStmt <*> genVoid
        , Export <$> genStmt <*> genVoid
        , Using <$> genFilePath <*> genVoid
        ]

genQualifiedStmt :: Gen (QualifiedStmt Stripped)
genQualifiedStmt =
    Gen.choice
        [ Plain <$> genDirective <*> genVoid
        , Override <$> genDirective <*> genVoid
        ]

genProgram :: Gen (BetzaProgram Stripped)
genProgram = Gen.sized $
    \(Size n) -> Gen.list (Range.linear 1 $ max 1 $ n `div` 20) genQualifiedStmt

-- utils

isOverride :: (QualifiedStmt p) -> Bool
isOverride (Override _ _) = True; isOverride _ = False
isPlain :: (QualifiedStmt p) -> Bool
isPlain (Plain _ _) = True; isPlain _ = False
isUsing :: (Directive p) -> Bool
isUsing (Using _ _) = True; isUsing _ = False
isExport :: (Directive p) -> Bool
isExport (Export _ _) = True; isExport _ = False
isBare :: (Directive p) -> Bool
isBare (Bare _ _) = True; isBare _ = False
isAssign :: (BetzaStmt p) -> Bool
isAssign (Assign _ _ _) = True; isAssign _ = False
isLabelRef :: (BetzaStmt p) -> Bool
isLabelRef (LabelRef _ _) = True; isLabelRef _ = False
isSequence :: (ChainKind p) -> Bool
isSequence (Sequence _) = True; isSequence _ = False

hasDirective :: (Directive p -> t) -> QualifiedStmt p -> t
hasDirective f (Override d _) = f d
hasDirective f (Plain d _) = f d

hasStmt :: (BetzaStmt p -> Bool) -> QualifiedStmt p -> Bool
hasStmt f qs = case qs of
    Plain (Bare s _) _ -> f s
    Plain (Export s _) _ -> f s
    Override (Bare s _) _ -> f s
    Override (Export s _) _ -> f s
    _ -> False

-- coverage utils

foldExpr ::
    (Monoid m) =>
    (ChainExpr p -> m) ->
    (ModifierExpr p -> m) ->
    (ExponentExpr p -> m) ->
    (AtomExpr p -> m) ->
    BetzaExpr p ->
    m
foldExpr fc fm fe fa (BetzaExpr c _) = goChain c
  where
    goChain ce@(ChainExpr (UnionExpr ms _) ml _) =
        fc ce
            <> foldMap goModifier (toList ms)
            <> maybe mempty (\(ChainLeg _ inner _) -> goChain inner) ml
    goModifier me@(ModifierExpr _ _ e _) =
        fm me <> goExponent e
    goExponent ee@(ExponentExpr a _ _) =
        fe ee <> goAtom a
    goAtom ae =
        fa ae <> case ae of
            Paren inner _ -> foldExpr fc fm fe fa inner
            From _ _ -> mempty

anyChainExpr :: (ChainExpr p -> Bool) -> BetzaExpr p -> Bool
anyChainExpr predicate = M.getAny . foldExpr (M.Any . predicate) mempty mempty mempty

anyModifierExpr :: (ModifierExpr p -> Bool) -> BetzaExpr p -> Bool
anyModifierExpr p = M.getAny . foldExpr mempty (M.Any . p) mempty mempty

anyExponentExpr :: (ExponentExpr p -> Bool) -> BetzaExpr p -> Bool
anyExponentExpr predicate = M.getAny . foldExpr mempty mempty (M.Any . predicate) mempty

anyAtomExpr :: (AtomExpr p -> Bool) -> BetzaExpr p -> Bool
anyAtomExpr predicate = M.getAny . foldExpr mempty mempty mempty (M.Any . predicate)

hasChainLeg :: BetzaExpr p -> Bool
hasChainLeg = anyChainExpr $ \(ChainExpr _ ml _) -> isJust ml

hasParenAtom :: BetzaExpr p -> Bool
hasParenAtom = anyAtomExpr $ \case Paren _ _ -> True; _ -> False

hasAtomExponent :: BetzaExpr p -> Bool
hasAtomExponent = anyExponentExpr $ \(ExponentExpr _ me _) -> isJust me

hasBang :: BetzaExpr p -> Bool
hasBang = anyModifierExpr $ \(ModifierExpr s _ _ _) -> s

hasAmalgamated :: forall p. (Qualifying p) => BetzaExpr p -> Bool
hasAmalgamated =
    anyModifierExpr $ \(ModifierExpr _ ms _ _) ->
        any (\case Directional (Amalgamated _ _ _) _ -> True; _ -> False) (toModifiers @p ms)

hasMultiUnion :: BetzaExpr p -> Bool
hasMultiUnion = anyChainExpr $ \(ChainExpr (UnionExpr ne _) _ _) -> length ne >= 2

hasModality :: (Qualifying p) => ChainModality p -> BetzaExpr p -> Bool
hasModality m =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg (ChainOperator _ m' _) _ _) -> m `stripEq` m') ml

hasSequence :: BetzaExpr p -> Bool
hasSequence =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg (ChainOperator k _ _) _ _) -> isSequence k) ml

hasNestedChain :: BetzaExpr p -> Bool
hasNestedChain =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg _ (ChainExpr _ ml' _) _) -> isJust ml') ml

anyInProg :: (BetzaExpr p -> Bool) -> BetzaProgram p -> Bool
anyInProg predicate = any (maybe False predicate . exprOf)
  where
    exprOf (Plain d _) = exprOfDirective d
    exprOf (Override d _) = exprOfDirective d
    exprOfDirective (Bare s _) = exprOfStmt s
    exprOfDirective (Export s _) = exprOfStmt s
    exprOfDirective (Using _ _) = Nothing
    exprOfStmt (Assign _ e _) = Just e
    exprOfStmt (LabelRef _ _) = Nothing

-- props

prop_parseUndoesUnparse :: PropertyT IO ()
prop_parseUndoesUnparse = do
    prog <- forAll genProgram
    let ts = unparse prog
        src = unlex ts
        stream = BetzaTokenStream src (liftLocated "<test>" <$> ts)
    annotate . show $ ts
    annotate src

    -- program-level stats
    classify "short (< 20 tokens)" $ length ts < 20
    classify "medium (20-100 tokens)" $ length ts >= 20 && length ts < 100
    classify "large (> 100 tokens)" $ length ts > 100

    classify "single statement" $ length prog == 1
    classify "multiple statements" $ length prog > 1

    -- qualified stmt
    classify "has Override" $ any isOverride prog
    classify "has Plain" $ any isPlain prog

    -- directive
    classify "has Using" $ any (hasDirective isUsing) prog
    classify "has Export" $ any (hasDirective isExport) prog
    classify "has Bare" $ any (hasDirective isBare) prog

    -- stmt
    classify "has Assign" $ any (hasStmt isAssign) prog
    classify "has LabelRef" $ any (hasStmt isLabelRef) prog

    -- chain structure
    classify "has chain leg" $ anyInProg hasChainLeg prog
    classify "has IffUnblocked leg" $ anyInProg (hasModality $ IffUnblocked ()) prog
    classify "has Choose leg" $ anyInProg (hasModality $ Choose ()) prog
    classify "has Sequence leg" $ anyInProg hasSequence prog
    classify "has Paren atom" $ anyInProg hasParenAtom prog
    classify "has exponent on atom" $ anyInProg hasAtomExponent prog
    classify "has nested chain" $ anyInProg hasNestedChain prog
    classify "has setup (bang)" $ anyInProg hasBang prog
    classify "has amalgamated direction" $ anyInProg hasAmalgamated prog
    classify "has union of 2+" $ anyInProg hasMultiUnion prog

    cover 10 "has chain leg" $ anyInProg hasChainLeg prog
    cover 5 "has IffUnblocked leg" $ anyInProg (hasModality $ IffUnblocked ()) prog
    cover 5 "has Choose leg" $ anyInProg (hasModality $ Choose ()) prog
    cover 5 "has Paren atom" $ anyInProg hasParenAtom prog
    cover 5 "has exponent on atom" $ anyInProg hasAtomExponent prog

    (map strip <$> parse parseTokens "<test>" stream) === Right prog

-- spec
spec :: Spec
spec = describe "Parser properties" $ do
    it "parse . unparse = id" $
        hedgehog $ do
            prop_parseUndoesUnparse
