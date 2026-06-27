{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserHedgehog (spec, genProgram, unparse) where

import Betzac.AST
import Betzac.Located
import Betzac.Parser.BetzaTokenStream
import Betzac.Parser.Parser
import qualified Betzac.Token as B
import Text.Megaparsec

import Lexer.LexerQC (unlex)

import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Monoid as M (Any (..), getAny)

import Data.List.NonEmpty (toList)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog

-- unparse

unparse :: BetzaProgram -> [B.Token]
unparse stmts = concat $ unparseQualifiedStmt <$> stmts

unparseQualifiedStmt :: QualifiedStmt -> [B.Token]
unparseQualifiedStmt (Override d) = B.TokOverride : unparseDirective d <> [B.TokEndStmt]
unparseQualifiedStmt (Plain d) = unparseDirective d <> [B.TokEndStmt]

unparseDirective :: Directive -> [B.Token]
unparseDirective (Using f) = [B.TokUsing $ unparseFilePath f]
unparseDirective (Export s) = B.TokExport : unparseStmt s
unparseDirective (Bare s) = unparseStmt s

unparseFilePath :: FilePath -> FilePath
unparseFilePath f = (\c -> if c == '/' then '.' else c) <$> f

unparseStmt :: BetzaStmt -> [B.Token]
unparseStmt (Assign l e) = unparseLabel l : B.TokAssign : unparseExpr e
unparseStmt (Anonymous e) = unparseExpr e

unparseLabel :: Label -> B.Token
unparseLabel (Upper c) = B.TokAtom c
unparseLabel (Descriptor s) = B.TokDescriptor s
unparseLabel (Leaper a b) = B.TokDescriptor $ show a ++ "," ++ show b

unparseExpr :: BetzaExpr -> [B.Token]
unparseExpr (BetzaExpr c) = unparseChain c

unparseChain :: ChainExpr -> [B.Token]
unparseChain (ChainExpr u mcl) =
    unparseUnion u
        <> maybe [] unparseChainLeg mcl

unparseChainLeg :: ChainLeg -> [B.Token]
unparseChainLeg (ChainLeg op c) = unparseChainOp op c

unparseChainOp :: ChainOperator -> ChainExpr -> [B.Token]
unparseChainOp (ChainOperator k Mandatory) c =
    unparseChainKind k : unparseChain c
unparseChainOp (ChainOperator k IffUnblocked) c =
    [unparseChainKind k, B.TokLBrace] <> unparseChain c <> [B.TokRBrace]
unparseChainOp (ChainOperator k Choose) c =
    [unparseChainKind k, B.TokLBracket] <> unparseChain c <> [B.TokRBracket]

unparseChainKind :: ChainKind -> B.Token
unparseChainKind Step = B.TokChainStep
unparseChainKind Sequence = B.TokChainSequence

-- unparseChainModality ??

unparseUnion :: UnionExpr -> [B.Token]
unparseUnion (UnionExpr ms) = concat $ unparseModExpr <$> ms

unparseModExpr :: ModifierExpr -> [B.Token]
unparseModExpr (ModifierExpr s ms e) =
    (if s then [B.TokBang] else [])
        <> (ms >>= unparseModifier)
        <> unparseExponentExpr e

unparseModifier :: Modifier -> [B.Token]
unparseModifier (Directional m) = unparseDirectionMod m
unparseModifier (Behavioural m) = unparseBehaviour m

unparseDirectionMod :: DirectionModifier -> [B.Token]
unparseDirectionMod (Amalgamated d1 d2) =
    [ B.TokLAngle
    , unparseDirection d1
    , unparseDirection d2
    , B.TokRAngle
    ]
unparseDirectionMod (Single d) = [unparseDirection d]

unparseDirection :: Direction -> B.Token
unparseDirection Forward = B.TokDirection 'f'
unparseDirection Backward = B.TokDirection 'b'
unparseDirection Leftward = B.TokDirection 'l'
unparseDirection Rightward = B.TokDirection 'r'
unparseDirection Sideway = B.TokDirection 's'
unparseDirection Vertically = B.TokDirection 'v'
unparseDirection All = B.TokDirection 'a'

unparseBehaviour :: Behaviour -> [B.Token]
unparseBehaviour (Behaviour kind Once) = [unparseBehaviourOnce kind]
unparseBehaviour (Behaviour kind Twice) = [unparseBehaviourOnce kind, unparseBehaviourOnce kind]
unparseBehaviour (Behaviour kind Any) = [unparseBehaviourOnce kind, B.TokBehaviour 'y']

unparseBehaviourOnce :: BehaviourKind -> B.Token
unparseBehaviourOnce Capture = B.TokBehaviour 'c'
unparseBehaviourOnce Leap = B.TokBehaviour 'g'
unparseBehaviourOnce Initial = B.TokBehaviour 'i'
unparseBehaviourOnce Jump = B.TokBehaviour 'j'
unparseBehaviourOnce Move = B.TokBehaviour 'm'
unparseBehaviourOnce NoJump = B.TokBehaviour 'n'
unparseBehaviourOnce Hop = B.TokBehaviour 'p'

unparseExponentExpr :: ExponentExpr -> [B.Token]
unparseExponentExpr (ExponentExpr a me) = unparseAtom a <> maybe [] unparseExponent me

unparseAtom :: AtomExpr -> [B.Token]
unparseAtom (Paren e) = [B.TokLParen] <> unparseExpr e <> [B.TokRParen]
unparseAtom (From l) = [unparseLabel l]

unparseExponent :: Exponent -> [B.Token]
unparseExponent (Exponent mop ms k) = case mop of
    Nothing -> (ms >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck Mandatory) -> [unparseChainKind ck] <> (ms >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck IffUnblocked) -> [unparseChainKind ck, B.TokLBrace] <> (ms >>= unparseModifier) <> [unparseExponentKind k, B.TokRBrace]
    Just (ChainOperator ck Choose) -> [unparseChainKind ck, B.TokLBracket] <> (ms >>= unparseModifier) <> [unparseExponentKind k, B.TokRBracket]

unparseExponentKind :: ExponentKind -> B.Token
unparseExponentKind Infinite = B.TokNumber 0
unparseExponentKind Slippery = B.TokSlippery
unparseExponentKind (Repeat n) = B.TokNumber n

-- generators

genDirection :: Gen Direction
genDirection = Gen.element [Forward, Backward, Leftward, Rightward, Sideway, Vertically, All]

genBehaviour :: Gen Behaviour
genBehaviour =
    Gen.choice
        [ Gen.element [Behaviour Initial Once, Behaviour Move Once, Behaviour NoJump Once]
        , Behaviour Capture <$> Gen.element [Once, Twice, Any]
        , Behaviour Leap <$> Gen.element [Once, Twice, Any]
        , Behaviour Jump <$> Gen.element [Once, Twice, Any]
        , Behaviour Hop <$> Gen.element [Once, Twice]
        ]

genChainOperator :: Gen ChainOperator
genChainOperator = ChainOperator <$> genChainKind <*> genChainModality

genChainKind :: Gen ChainKind
genChainKind = Gen.element [Step, Sequence]

genChainModality :: Gen ChainModality
genChainModality = Gen.element [Mandatory, Choose, IffUnblocked]

genNumber :: Gen Number
genNumber = Gen.sized $ \(Size n) -> Gen.int $ Range.linear 1 (max 1 n)

genExponentKind :: Gen ExponentKind
genExponentKind = Gen.choice [Gen.constant Infinite, Gen.constant Slippery, Repeat <$> genNumber]

genUpper :: Gen Label
genUpper = Upper <$> Gen.element ['A' .. 'Z']

genDescriptor :: Gen Label
genDescriptor =
    Descriptor <$> do
        wrds <-
            Gen.nonEmpty (Range.linear 1 5) $
                Gen.string (Range.linear 1 8) Gen.alphaNum
        pure $ intercalate " " (toList wrds)

genLeaper :: Gen Label
genLeaper = Leaper <$> genNumber <*> genNumber

genLabel :: Gen Label
genLabel = Gen.choice [genUpper, genDescriptor, genLeaper]

genDirectionMod :: Gen DirectionModifier
genDirectionMod =
    Gen.frequency
        [ (2, Single <$> genDirection)
        , (1, Amalgamated <$> genDirection <*> genDirection)
        ]

genModifier :: Gen Modifier
genModifier = Gen.choice [Directional <$> genDirectionMod, Behavioural <$> genBehaviour]

genModifiers :: Gen [Modifier]
genModifiers =
    Gen.filter noAmbiguousSequences $
        Gen.list (Range.linear 0 3) genModifier
  where
    noAmbiguousSequences ms =
        let ts = ms >>= unparseModifier
         in not $ any ambiguous $ zip ts $ drop 1 ts
    ambiguous (B.TokBehaviour a, B.TokBehaviour b) = a == b || b == 'y'
    ambiguous _ = False

genSmallExpr :: Gen BetzaExpr
genSmallExpr = Gen.sized $ \n -> Gen.resize (n `div` 2) genExpr

genAtom :: Gen AtomExpr
genAtom =
    Gen.recursive
        Gen.choice
        [From <$> genLabel]
        [Paren <$> genSmallExpr]

genExponent :: Gen Exponent
genExponent =
    Exponent
        <$> Gen.maybe genChainOperator
        <*> genModifiers
        <*> genExponentKind

genExponentExpr :: Gen ExponentExpr
genExponentExpr = ExponentExpr <$> genAtom <*> (Gen.maybe genExponent)

genModifierExpr :: Gen ModifierExpr
genModifierExpr =
    ModifierExpr
        <$> Gen.bool -- setup
        <*> genModifiers
        <*> genExponentExpr

genUnion :: Gen UnionExpr
genUnion = UnionExpr <$> Gen.nonEmpty (Range.linear 1 4) genModifierExpr

genChain :: Gen ChainExpr
genChain =
    Gen.recursive
        Gen.choice
        [ChainExpr <$> genUnion <*> return Nothing]
        [ChainExpr <$> genUnion <*> (Just <$> genChainLeg)]

genChainLeg :: Gen ChainLeg
genChainLeg = ChainLeg <$> genChainOperator <*> genSmallChain
  where
    genSmallChain = Gen.sized $ \n -> Gen.resize (n `div` 2) genChain

genExpr :: Gen BetzaExpr
genExpr = BetzaExpr <$> genChain

genStmt :: Gen BetzaStmt
genStmt =
    Gen.choice
        [ Anonymous <$> genExpr
        , Assign <$> genLabel <*> genExpr
        ]

genFilePath :: Gen FilePath
genFilePath =
    intercalate "/"
        <$> Gen.list
            (Range.linear 1 10)
            (Gen.string (Range.linear 1 20) Gen.alphaNum)

genDirective :: Gen Directive
genDirective =
    Gen.choice
        [ Bare <$> genStmt
        , Export <$> genStmt
        , Using <$> genFilePath
        ]

genQualifiedStmt :: Gen QualifiedStmt
genQualifiedStmt =
    Gen.choice
        [ Plain <$> genDirective
        , Override <$> genDirective
        ]

genProgram :: Gen BetzaProgram
genProgram = Gen.sized $
    \(Size n) -> Gen.list (Range.linear 1 (max 1 (n `div` 20))) genQualifiedStmt

-- utils

isOverride :: QualifiedStmt -> Bool
isOverride (Override _) = True; isOverride _ = False
isPlain :: QualifiedStmt -> Bool
isPlain (Plain _) = True; isPlain _ = False
isUsing :: Directive -> Bool
isUsing (Using _) = True; isUsing _ = False
isExport :: Directive -> Bool
isExport (Export _) = True; isExport _ = False
isBare :: Directive -> Bool
isBare (Bare _) = True; isBare _ = False
isAssign :: BetzaStmt -> Bool
isAssign (Assign{}) = True; isAssign _ = False
isAnonymous :: BetzaStmt -> Bool
isAnonymous (Anonymous _) = True; isAnonymous _ = False

hasDirective :: (Directive -> t) -> QualifiedStmt -> t
hasDirective f (Override d) = f d
hasDirective f (Plain d) = f d

hasStmt :: (BetzaStmt -> Bool) -> QualifiedStmt -> Bool
hasStmt f qs = case qs of
    Plain (Bare s) -> f s
    Plain (Export s) -> f s
    Override (Bare s) -> f s
    Override (Export s) -> f s
    _ -> False

-- coverage utils

foldExpr ::
    (Monoid m) =>
    (ChainExpr -> m) ->
    (ModifierExpr -> m) ->
    (ExponentExpr -> m) ->
    (AtomExpr -> m) ->
    BetzaExpr ->
    m
foldExpr fc fm fe fa (BetzaExpr c) = goChain c
  where
    goChain ce@(ChainExpr (UnionExpr ms) ml) =
        fc ce
            <> foldMap goModifier (toList ms)
            <> maybe mempty (\(ChainLeg _ inner) -> goChain inner) ml
    goModifier me@(ModifierExpr _ _ e) =
        fm me <> goExponent e
    goExponent ee@(ExponentExpr a _) =
        fe ee <> goAtom a
    goAtom ae =
        fa ae <> case ae of
            Paren inner -> foldExpr fc fm fe fa inner
            From _ -> mempty

anyChainExpr :: (ChainExpr -> Bool) -> BetzaExpr -> Bool
anyChainExpr p = M.getAny . foldExpr (M.Any . p) mempty mempty mempty

anyModifierExpr :: (ModifierExpr -> Bool) -> BetzaExpr -> Bool
anyModifierExpr p = M.getAny . foldExpr mempty (M.Any . p) mempty mempty

anyExponentExpr :: (ExponentExpr -> Bool) -> BetzaExpr -> Bool
anyExponentExpr p = M.getAny . foldExpr mempty mempty (M.Any . p) mempty

anyAtomExpr :: (AtomExpr -> Bool) -> BetzaExpr -> Bool
anyAtomExpr p = M.getAny . foldExpr mempty mempty mempty (M.Any . p)

hasChainLeg :: BetzaExpr -> Bool
hasChainLeg = anyChainExpr (\(ChainExpr _ ml) -> isJust ml)

hasParenAtom :: BetzaExpr -> Bool
hasParenAtom = anyAtomExpr (\case Paren _ -> True; _ -> False)

hasAtomExponent :: BetzaExpr -> Bool
hasAtomExponent = anyExponentExpr (\(ExponentExpr _ me) -> isJust me)

hasBang :: BetzaExpr -> Bool
hasBang = anyModifierExpr (\(ModifierExpr s _ _) -> s)

hasAmalgamated :: BetzaExpr -> Bool
hasAmalgamated =
    anyModifierExpr
        ( \(ModifierExpr _ ms _) ->
            any (\case Directional (Amalgamated _ _) -> True; _ -> False) ms
        )

hasMultiUnion :: BetzaExpr -> Bool
hasMultiUnion = anyChainExpr (\(ChainExpr (UnionExpr ne) _) -> length ne >= 2)

hasModality :: ChainModality -> BetzaExpr -> Bool
hasModality m =
    anyChainExpr
        ( \(ChainExpr _ ml) ->
            maybe False (\(ChainLeg (ChainOperator _ m') _) -> m' == m) ml
        )

hasSequence :: BetzaExpr -> Bool
hasSequence =
    anyChainExpr
        ( \(ChainExpr _ ml) ->
            maybe False (\(ChainLeg (ChainOperator k _) _) -> k == Sequence) ml
        )

hasNestedChain :: BetzaExpr -> Bool
hasNestedChain =
    anyChainExpr
        ( \(ChainExpr _ ml) ->
            maybe False (\(ChainLeg _ (ChainExpr _ ml')) -> isJust ml') ml
        )

anyInProg :: (BetzaExpr -> Bool) -> BetzaProgram -> Bool
anyInProg p = any (maybe False p . exprOf)
  where
    exprOf (Plain d) = exprOfDirective d
    exprOf (Override d) = exprOfDirective d
    exprOfDirective (Bare s) = exprOfStmt s
    exprOfDirective (Export s) = exprOfStmt s
    exprOfDirective (Using _) = Nothing
    exprOfStmt (Assign _ e) = Just e
    exprOfStmt (Anonymous e) = Just e

-- props

prop_parseUndoesUnparse :: PropertyT IO ()
prop_parseUndoesUnparse = do
    prog <- forAll genProgram
    let ts = unparse prog
        src = unlex ts
        stream = BetzaTokenStream src (liftLocated "<test>" <$> ts)
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
    classify "has Anonymous" $ any (hasStmt isAnonymous) prog

    -- chain structure
    classify "has chain leg" $ anyInProg hasChainLeg prog
    classify "has IffUnblocked leg" $ anyInProg (hasModality IffUnblocked) prog
    classify "has Choose leg" $ anyInProg (hasModality Choose) prog
    classify "has Sequence leg" $ anyInProg hasSequence prog
    classify "has Paren atom" $ anyInProg hasParenAtom prog
    classify "has exponent on atom" $ anyInProg hasAtomExponent prog
    classify "has nested chain" $ anyInProg hasNestedChain prog
    classify "has setup (bang)" $ anyInProg hasBang prog
    classify "has amalgamated direction" $ anyInProg hasAmalgamated prog
    classify "has union of 2+" $ anyInProg hasMultiUnion prog

    cover 10 "has chain leg" $ anyInProg hasChainLeg prog
    cover 5 "has IffUnblocked leg" $ anyInProg (hasModality IffUnblocked) prog
    cover 5 "has Choose leg" $ anyInProg (hasModality Choose) prog
    cover 5 "has Paren atom" $ anyInProg hasParenAtom prog
    cover 5 "has exponent on atom" $ anyInProg hasAtomExponent prog

    (parse parseTokens "<test>" stream) === Right prog

-- spec
spec :: Spec
spec = describe "Parser properties" $ do
    it "parse . unparse = id" $
        hedgehog $ do
            prop_parseUndoesUnparse