{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserHedgehog (spec, genProgram, unparse) where

import Betzac.AST
import Betzac.Debug.Strip (strip)
import Betzac.Located
import Betzac.Parser.BetzaTokenStream
import Betzac.Parser.Parser
import qualified Betzac.Token as B

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

-- unparse

unparse :: BetzaProgram Ps -> [B.Token]
unparse stmts = concat $ unparseQualifiedStmt <$> stmts

unparseQualifiedStmt :: QualifiedStmt Ps -> [B.Token]
unparseQualifiedStmt (Override d _) = B.TokOverride : unparseDirective d <> [B.TokEndStmt]
unparseQualifiedStmt (Plain d _) = unparseDirective d <> [B.TokEndStmt]

unparseDirective :: Directive Ps -> [B.Token]
unparseDirective (Using f _) = [B.TokUsing $ unparseFilePath f]
unparseDirective (Export s _) = B.TokExport : unparseStmt s
unparseDirective (Bare s _) = unparseStmt s

unparseFilePath :: FilePath -> FilePath
unparseFilePath f = (\c -> if c == '/' then '.' else c) <$> f

unparseStmt :: BetzaStmt Ps -> [B.Token]
unparseStmt (Assign l e _) = unparseLabel l : B.TokAssign : unparseExpr e
unparseStmt (Anonymous e _) = unparseExpr e

unparseLabel :: Label Ps -> B.Token
unparseLabel (Upper c _) = B.TokAtom c
unparseLabel (Descriptor s _) = B.TokDescriptor s
unparseLabel (Leaper a b _) = B.TokDescriptor $ show a ++ "," ++ show b

unparseExpr :: BetzaExpr Ps -> [B.Token]
unparseExpr (BetzaExpr c _) = unparseChain c

unparseChain :: ChainExpr Ps -> [B.Token]
unparseChain (ChainExpr u mcl _) = unparseUnion u <> maybe [] unparseChainLeg mcl

unparseChainLeg :: ChainLeg Ps -> [B.Token]
unparseChainLeg (ChainLeg op c _) = unparseChainOp op c

unparseChainOp :: ChainOperator Ps -> ChainExpr Ps -> [B.Token]
unparseChainOp (ChainOperator k (Mandatory _) _) c =
    unparseChainKind k : unparseChain c
unparseChainOp (ChainOperator k (IffUnblocked _) _) c =
    [unparseChainKind k, B.TokLBrace] <> unparseChain c <> [B.TokRBrace]
unparseChainOp (ChainOperator k (Choose _) _) c =
    [unparseChainKind k, B.TokLBracket] <> unparseChain c <> [B.TokRBracket]

unparseChainKind :: ChainKind Ps -> B.Token
unparseChainKind (Step _) = B.TokChainStep
unparseChainKind (Sequence _) = B.TokChainSequence

unparseUnion :: UnionExpr Ps -> [B.Token]
unparseUnion (UnionExpr ms _) = concat $ unparseModExpr <$> ms

unparseModExpr :: ModifierExpr Ps -> [B.Token]
unparseModExpr (ModifierExpr s ms e _) =
    (if s then [B.TokBang] else [])
        <> (ms >>= unparseModifier)
        <> unparseExponentExpr e

unparseModifier :: Modifier Ps -> [B.Token]
unparseModifier (Directional m _) = unparseDirectionMod m
unparseModifier (Behavioural m _) = unparseBehaviour m

unparseDirectionMod :: DirectionModifier Ps -> [B.Token]
unparseDirectionMod (Amalgamated d1 d2 _) =
    [ B.TokLAngle
    , unparseDirection d1
    , unparseDirection d2
    , B.TokRAngle
    ]
unparseDirectionMod (Single d _) = [unparseDirection d]

unparseDirection :: Direction Ps -> B.Token
unparseDirection (Forward _) = B.TokDirection 'f'
unparseDirection (Backward _) = B.TokDirection 'b'
unparseDirection (Leftward _) = B.TokDirection 'l'
unparseDirection (Rightward _) = B.TokDirection 'r'
unparseDirection (Sideway _) = B.TokDirection 's'
unparseDirection (Vertically _) = B.TokDirection 'v'
unparseDirection (All _) = B.TokDirection 'a'

unparseBehaviour :: Behaviour Ps -> [B.Token]
unparseBehaviour (Behaviour kind (Once _) _) = [unparseBehaviourOnce kind]
unparseBehaviour (Behaviour kind (Twice _) _) = [unparseBehaviourOnce kind, unparseBehaviourOnce kind]
unparseBehaviour (Behaviour kind (Any _) _) = [unparseBehaviourOnce kind, B.TokBehaviour 'y']

unparseBehaviourOnce :: BehaviourKind Ps -> B.Token
unparseBehaviourOnce (Capture _) = B.TokBehaviour 'c'
unparseBehaviourOnce (Leap _) = B.TokBehaviour 'g'
unparseBehaviourOnce (Initial _) = B.TokBehaviour 'i'
unparseBehaviourOnce (Jump _) = B.TokBehaviour 'j'
unparseBehaviourOnce (Move _) = B.TokBehaviour 'm'
unparseBehaviourOnce (NoJump _) = B.TokBehaviour 'n'
unparseBehaviourOnce (Hop _) = B.TokBehaviour 'p'

unparseExponentExpr :: ExponentExpr Ps -> [B.Token]
unparseExponentExpr (ExponentExpr a me _) = unparseAtom a <> maybe [] unparseExponent me

unparseAtom :: AtomExpr Ps -> [B.Token]
unparseAtom (Paren e _) = [B.TokLParen] <> unparseExpr e <> [B.TokRParen]
unparseAtom (From l _) = [unparseLabel l]

unparseExponent :: Exponent Ps -> [B.Token]
unparseExponent (Exponent mop ms k _) = case mop of
    Nothing -> (ms >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck (Mandatory _) _) -> [unparseChainKind ck] <> (ms >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck (IffUnblocked _) _) -> [unparseChainKind ck, B.TokLBrace] <> (ms >>= unparseModifier) <> [unparseExponentKind k, B.TokRBrace]
    Just (ChainOperator ck (Choose _) _) -> [unparseChainKind ck, B.TokLBracket] <> (ms >>= unparseModifier) <> [unparseExponentKind k, B.TokRBracket]

unparseExponentKind :: ExponentKind Ps -> B.Token
unparseExponentKind (Infinite _) = B.TokNumber 0
unparseExponentKind (Slippery _) = B.TokSlippery
unparseExponentKind (Repeat n _) = B.TokNumber n

-- generators

psx :: PsX -- dummy
psx = PsX Generated

genPsx :: Gen PsX
genPsx = Gen.constant psx

genDirection :: Gen (Direction Ps)
genDirection =
    Gen.element
        [ Forward psx
        , Backward psx
        , Leftward psx
        , Rightward psx
        , Sideway psx
        , Vertically psx
        , All psx
        ]

genBehaviour :: Gen (Behaviour Ps)
genBehaviour =
    Gen.choice
        [ Gen.element
            [ Behaviour (Initial psx) (Once ()) psx
            , Behaviour (Move psx) (Once ()) psx
            , Behaviour (NoJump psx) (Once ()) psx
            ]
        , Behaviour (Capture psx) <$> Gen.element [Once (), Twice psx, Any psx] <*> genPsx
        , Behaviour (Leap psx) <$> Gen.element [Once (), Twice psx, Any psx] <*> genPsx
        , Behaviour (Jump psx) <$> Gen.element [Once (), Twice psx, Any psx] <*> genPsx
        , Behaviour (Hop psx) <$> Gen.element [Once (), Twice psx, Any psx] <*> genPsx
        ]

genChainOperator :: Gen (ChainOperator Ps)
genChainOperator = ChainOperator <$> genChainKind <*> genChainModality <*> genPsx

genChainKind :: Gen (ChainKind Ps)
genChainKind = Gen.element [Step psx, Sequence psx]

genChainModality :: Gen (ChainModality Ps)
genChainModality = Gen.element [Mandatory (), Choose psx, IffUnblocked psx]

genNumber :: Gen Number
genNumber = Gen.sized $ \(Size n) -> Gen.int $ Range.linear 1 (max 1 n)

genExponentKind :: Gen (ExponentKind Ps)
genExponentKind =
    Gen.choice
        [ Infinite <$> genPsx
        , Slippery <$> genPsx
        , Repeat <$> genNumber <*> genPsx
        ]

genUpper :: Gen (Label Ps)
genUpper = Upper <$> Gen.element ['A' .. 'Z'] <*> genPsx

genDescriptor :: Gen (Label Ps)
genDescriptor =
    Descriptor
        <$> do
            wrds <-
                Gen.nonEmpty (Range.linear 1 5) $
                    Gen.string (Range.linear 1 8) Gen.alphaNum
            pure $ intercalate " " (toList wrds)
        <*> genPsx

genLeaper :: Gen (Label Ps)
genLeaper = Leaper <$> genNumber <*> genNumber <*> genPsx

genLabel :: Gen (Label Ps)
genLabel = Gen.choice [genUpper, genDescriptor, genLeaper]

genDirectionMod :: Gen (DirectionModifier Ps)
genDirectionMod =
    Gen.frequency
        [ (2, Single <$> genDirection <*> genPsx)
        , (1, Amalgamated <$> genDirection <*> genDirection <*> genPsx)
        ]

genModifier :: Gen (Modifier Ps)
genModifier =
    Gen.choice
        [ Directional <$> genDirectionMod <*> genPsx
        , Behavioural <$> genBehaviour <*> genPsx
        ]

genModifiers :: Gen [Modifier Ps]
genModifiers =
    Gen.filter noAmbiguousSequences $
        Gen.list (Range.linear 0 3) genModifier
  where
    noAmbiguousSequences ms =
        let ts = ms >>= unparseModifier
         in not $ any ambiguous $ zip ts $ drop 1 ts
    ambiguous (B.TokBehaviour a, B.TokBehaviour b) = a == b || b == 'y'
    ambiguous _ = False

genSmallExpr :: Gen (BetzaExpr Ps)
genSmallExpr = Gen.sized $ \n -> Gen.resize (n `div` 2) genExpr

genAtom :: Gen (AtomExpr Ps)
genAtom =
    Gen.recursive
        Gen.choice
        [From <$> genLabel <*> genPsx]
        [Paren <$> genSmallExpr <*> genPsx]

genExponent :: Gen (Exponent Ps)
genExponent =
    Exponent
        <$> Gen.maybe genChainOperator
        <*> genModifiers
        <*> genExponentKind
        <*> genPsx

genExponentExpr :: Gen (ExponentExpr Ps)
genExponentExpr =
    ExponentExpr
        <$> genAtom
        <*> (Gen.maybe genExponent)
        <*> genPsx

genModifierExpr :: Gen (ModifierExpr Ps)
genModifierExpr =
    ModifierExpr
        <$> Gen.bool -- setup
        <*> genModifiers
        <*> genExponentExpr
        <*> genPsx

genUnion :: Gen (UnionExpr Ps)
genUnion = UnionExpr <$> Gen.nonEmpty (Range.linear 1 4) genModifierExpr <*> genPsx

genChain :: Gen (ChainExpr Ps)
genChain =
    Gen.recursive
        Gen.choice
        [ChainExpr <$> genUnion <*> return Nothing <*> genPsx]
        [ChainExpr <$> genUnion <*> (Just <$> genChainLeg) <*> genPsx]

genChainLeg :: Gen (ChainLeg Ps)
genChainLeg = ChainLeg <$> genChainOperator <*> genSmallChain <*> genPsx
  where
    genSmallChain = Gen.sized $ \n -> Gen.resize (n `div` 2) genChain

genExpr :: Gen (BetzaExpr Ps)
genExpr = BetzaExpr <$> genChain <*> genPsx

genStmt :: Gen (BetzaStmt Ps)
genStmt =
    Gen.choice
        [ Anonymous <$> genExpr <*> genPsx
        , Assign <$> genLabel <*> genExpr <*> genPsx
        ]

genFilePath :: Gen FilePath
genFilePath =
    intercalate "/"
        <$> Gen.list
            (Range.linear 1 10)
            (Gen.string (Range.linear 1 20) Gen.alphaNum)

genDirective :: Gen (Directive Ps)
genDirective =
    Gen.choice
        [ Bare <$> genStmt <*> genPsx
        , Export <$> genStmt <*> genPsx
        , Using <$> genFilePath <*> genPsx
        ]

genQualifiedStmt :: Gen (QualifiedStmt Ps)
genQualifiedStmt =
    Gen.choice
        [ Plain <$> genDirective <*> genPsx
        , Override <$> genDirective <*> genPsx
        ]

genProgram :: Gen (BetzaProgram Ps)
genProgram = Gen.sized $
    \(Size n) -> Gen.list (Range.linear 1 $ max 1 $ n `div` 20) genQualifiedStmt

-- utils

isOverride :: (QualifiedStmt Ps) -> Bool
isOverride (Override _ _) = True; isOverride _ = False
isPlain :: (QualifiedStmt Ps) -> Bool
isPlain (Plain _ _) = True; isPlain _ = False
isUsing :: (Directive Ps) -> Bool
isUsing (Using _ _) = True; isUsing _ = False
isExport :: (Directive Ps) -> Bool
isExport (Export _ _) = True; isExport _ = False
isBare :: (Directive Ps) -> Bool
isBare (Bare _ _) = True; isBare _ = False
isAssign :: (BetzaStmt Ps) -> Bool
isAssign (Assign _ _ _) = True; isAssign _ = False
isAnonymous :: (BetzaStmt Ps) -> Bool
isAnonymous (Anonymous _ _) = True; isAnonymous _ = False
isSequence :: (ChainKind Ps) -> Bool
isSequence (Sequence _) = True; isSequence _ = False

hasDirective :: (Directive Ps -> t) -> QualifiedStmt Ps -> t
hasDirective f (Override d _) = f d
hasDirective f (Plain d _) = f d

hasStmt :: (BetzaStmt Ps -> Bool) -> QualifiedStmt Ps -> Bool
hasStmt f qs = case qs of
    Plain (Bare s _) _ -> f s
    Plain (Export s _) _ -> f s
    Override (Bare s _) _ -> f s
    Override (Export s _) _ -> f s
    _ -> False

-- coverage utils

foldExpr ::
    (Monoid m) =>
    (ChainExpr Ps -> m) ->
    (ModifierExpr Ps -> m) ->
    (ExponentExpr Ps -> m) ->
    (AtomExpr Ps -> m) ->
    BetzaExpr Ps ->
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

anyChainExpr :: (ChainExpr Ps -> Bool) -> BetzaExpr Ps -> Bool
anyChainExpr p = M.getAny . foldExpr (M.Any . p) mempty mempty mempty

anyModifierExpr :: (ModifierExpr Ps -> Bool) -> BetzaExpr Ps -> Bool
anyModifierExpr p = M.getAny . foldExpr mempty (M.Any . p) mempty mempty

anyExponentExpr :: (ExponentExpr Ps -> Bool) -> BetzaExpr Ps -> Bool
anyExponentExpr p = M.getAny . foldExpr mempty mempty (M.Any . p) mempty

anyAtomExpr :: (AtomExpr Ps -> Bool) -> BetzaExpr Ps -> Bool
anyAtomExpr p = M.getAny . foldExpr mempty mempty mempty (M.Any . p)

hasChainLeg :: BetzaExpr Ps -> Bool
hasChainLeg = anyChainExpr $ \(ChainExpr _ ml _) -> isJust ml

hasParenAtom :: BetzaExpr Ps -> Bool
hasParenAtom = anyAtomExpr $ \case Paren _ _ -> True; _ -> False

hasAtomExponent :: BetzaExpr Ps -> Bool
hasAtomExponent = anyExponentExpr $ \(ExponentExpr _ me _) -> isJust me

hasBang :: BetzaExpr Ps -> Bool
hasBang = anyModifierExpr $ \(ModifierExpr s _ _ _) -> s

hasAmalgamated :: BetzaExpr Ps -> Bool
hasAmalgamated =
    anyModifierExpr $ \(ModifierExpr _ ms _ _) ->
        any (\case Directional (Amalgamated _ _ _) _ -> True; _ -> False) ms

hasMultiUnion :: BetzaExpr Ps -> Bool
hasMultiUnion = anyChainExpr $ \(ChainExpr (UnionExpr ne _) _ _) -> length ne >= 2

hasModality :: ChainModality Ps -> BetzaExpr Ps -> Bool
hasModality m =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg (ChainOperator _ m' _) _ _) -> m' == m) ml

hasSequence :: BetzaExpr Ps -> Bool
hasSequence =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg (ChainOperator k _ _) _ _) -> isSequence k) ml

hasNestedChain :: BetzaExpr Ps -> Bool
hasNestedChain =
    anyChainExpr $ \(ChainExpr _ ml _) ->
        maybe False (\(ChainLeg _ (ChainExpr _ ml' _) _) -> isJust ml') ml

anyInProg :: (BetzaExpr Ps -> Bool) -> BetzaProgram Ps -> Bool
anyInProg p = any (maybe False p . exprOf)
  where
    exprOf (Plain d _) = exprOfDirective d
    exprOf (Override d _) = exprOfDirective d
    exprOfDirective (Bare s _) = exprOfStmt s
    exprOfDirective (Export s _) = exprOfStmt s
    exprOfDirective (Using _ _) = Nothing
    exprOfStmt (Assign _ e _) = Just e
    exprOfStmt (Anonymous e _) = Just e

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
    classify "has IffUnblocked leg" $ anyInProg (hasModality $ IffUnblocked psx) prog
    classify "has Choose leg" $ anyInProg (hasModality $ Choose psx) prog
    classify "has Sequence leg" $ anyInProg hasSequence prog
    classify "has Paren atom" $ anyInProg hasParenAtom prog
    classify "has exponent on atom" $ anyInProg hasAtomExponent prog
    classify "has nested chain" $ anyInProg hasNestedChain prog
    classify "has setup (bang)" $ anyInProg hasBang prog
    classify "has amalgamated direction" $ anyInProg hasAmalgamated prog
    classify "has union of 2+" $ anyInProg hasMultiUnion prog

    cover 10 "has chain leg" $ anyInProg hasChainLeg prog
    cover 5 "has IffUnblocked leg" $ anyInProg (hasModality $ IffUnblocked psx) prog
    cover 5 "has Choose leg" $ anyInProg (hasModality $ Choose psx) prog
    cover 5 "has Paren atom" $ anyInProg hasParenAtom prog
    cover 5 "has exponent on atom" $ anyInProg hasAtomExponent prog

    case parse parseTokens "<test>" stream of
        Left err -> footnote (show err) >> Test.Hspec.Hedgehog.failure
        Right prog' -> strip prog' === strip prog

-- spec
spec :: Spec
spec = describe "Parser properties" $ do
    it "parse . unparse = id" $
        hedgehog $ do
            prop_parseUndoesUnparse