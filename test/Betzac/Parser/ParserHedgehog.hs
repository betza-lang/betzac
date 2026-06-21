{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserHedgehog (spec) where

import Betzac.AST
import Betzac.Parser.Core (runParser)
import Betzac.Parser.Parser (parseTokens)
import Betzac.Token

import Betzac.Lexer.ErrorHandling (emptyTokenMap)
import Lexer.LexerQC (unlex)

import Data.List (intercalate, singleton)

import Data.List.NonEmpty (toList)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog

-- unparse

unparse :: BetzaProgram -> [Token]
unparse stmts = concat $ unparseQualifiedStmt <$> stmts

unparseQualifiedStmt :: QualifiedStmt -> [Token]
unparseQualifiedStmt (Override d) = TokOverride : unparseDirective d <> [TokEndStmt]
unparseQualifiedStmt (Plain d) = unparseDirective d <> [TokEndStmt]

unparseDirective :: Directive -> [Token]
unparseDirective (Using f) = [TokUsing $ unparseFilePath f]
unparseDirective (Export s) = TokExport : unparseStmt s
unparseDirective (Bare s) = unparseStmt s

unparseFilePath :: FilePath -> FilePath
unparseFilePath f = (\c -> if c == '/' then '.' else c) <$> f

unparseStmt :: BetzaStmt -> [Token]
unparseStmt (Assign l e) = unparseLabel l : TokAssign : unparseExpr e
unparseStmt (Anonymous e) = unparseExpr e

unparseLabel :: Label -> Token
unparseLabel (Upper c) = TokAtom c
unparseLabel (Descriptor s) = TokDescriptor s
unparseLabel (Leaper a b) = TokDescriptor $ show a ++ "," ++ show b

unparseExpr :: BetzaExpr -> [Token]
unparseExpr (BetzaExpr c) = unparseChain c

unparseChain :: ChainExpr -> [Token]
unparseChain (ChainExpr o t) = unparseOption o <> unparseChainTail t

unparseChainTail :: [(ChainOperator, OptionExpr)] -> [Token]
unparseChainTail = concatMap $ \(op, o) -> unparseChainOp op : unparseOption o

unparseChainOp :: ChainOperator -> Token
unparseChainOp Step = TokChainStep
unparseChainOp Sequence = TokChainSequence

unparseOption :: OptionExpr -> [Token]
unparseOption (Choose e) = [TokLBracket] <> unparseExpr e <> [TokRBracket]
unparseOption (IffUnblocked e) = [TokLBrace] <> unparseExpr e <> [TokRBrace]
unparseOption (Mandatory u) = unparseUnion u

unparseUnion :: UnionExpr -> [Token]
unparseUnion (UnionExpr ms) = concat $ unparseModExpr <$> ms

unparseModExpr :: ModifierExpr -> [Token]
unparseModExpr (ModifierExpr s ms e) =
    (if s then [TokBang] else [])
        <> (ms >>= unparseModifier)
        <> unparseExponentExpr e

unparseModifier :: Modifier -> [Token]
unparseModifier (Directional m) = unparseDirectionMod m
unparseModifier (Behavioural m) = [unparseBehaviour m]

unparseDirectionMod :: DirectionModifier -> [Token]
unparseDirectionMod (Amalgamated d1 d2) =
    [ TokLAngle
    , unparseDirection d1
    , unparseDirection d2
    , TokRAngle
    ]
unparseDirectionMod (Single d) = [unparseDirection d]

unparseDirection :: Direction -> Token
unparseDirection Forward = TokDirection 'f'
unparseDirection Backward = TokDirection 'b'
unparseDirection Leftward = TokDirection 'l'
unparseDirection Rightward = TokDirection 'r'
unparseDirection Sideway = TokDirection 's'
unparseDirection Vertically = TokDirection 'v'
unparseDirection All = TokDirection 'a'

unparseBehaviour :: Behaviour -> Token
unparseBehaviour Capture = TokBehaviour 'c'
unparseBehaviour Leap = TokBehaviour 'g'
unparseBehaviour Initial = TokBehaviour 'i'
unparseBehaviour Jump = TokBehaviour 'j'
unparseBehaviour Move = TokBehaviour 'm'
unparseBehaviour NoJump = TokBehaviour 'n'
unparseBehaviour Hop = TokBehaviour 'p'
unparseBehaviour Any = TokBehaviour 'y'

unparseExponentExpr :: ExponentExpr -> [Token]
unparseExponentExpr (ExponentExpr a me) = unparseAtom a <> maybe [] unparseExponent me

unparseAtom :: AtomExpr -> [Token]
unparseAtom (Paren e) = [TokLParen] <> unparseExpr e <> [TokRParen]
unparseAtom (From l) = [unparseLabel l]

unparseExponent :: Exponent -> [Token]
unparseExponent (Exponent mop ms k) =
    maybe [] (singleton . unparseChainOp) mop
        <> (ms >>= unparseModifier)
        <> [unparseExponentKind k]

unparseExponentKind :: ExponentKind -> Token
unparseExponentKind Infinite = TokNumber 0
unparseExponentKind Slippery = TokSlippery
unparseExponentKind (Repeat n) = TokNumber n

-- generators

genDirection :: Gen Direction
genDirection = Gen.element [Forward, Backward, Leftward, Rightward, Sideway, Vertically, All]

genBehaviour :: Gen Behaviour
genBehaviour = Gen.element [Capture, Leap, Initial, Jump, Move, NoJump, Hop, Any]

genChainOperator :: Gen ChainOperator
genChainOperator = Gen.element [Step, Sequence]

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
genModifiers = Gen.list (Range.linear 0 3) genModifier

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

genOption :: Gen OptionExpr
genOption =
    Gen.sized $ \n ->
        Gen.frequency $
            [(2, Mandatory <$> genUnion)]
                <> if n <= 1
                    then []
                    else
                        [ (1, Choose <$> genSmallExpr)
                        , (1, IffUnblocked <$> genSmallExpr)
                        ]

genChain :: Gen ChainExpr
genChain = ChainExpr <$> genOption <*> genChainTail

genChainTail :: Gen [(ChainOperator, OptionExpr)]
genChainTail = Gen.list (Range.linear 0 3) ((,) <$> genChainOperator <*> genOption)

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

-- props

prop_parseUndoesUnparse :: PropertyT IO ()
prop_parseUndoesUnparse = do
    prog <- forAll genProgram
    annotate . unlex . unparse $ prog

    -- program-level stats
    let tokens = unparse prog
    classify "short (< 20 tokens)" $ length tokens < 20
    classify "medium (20-100 tokens)" $ length tokens >= 20 && length tokens < 100
    classify "large (> 100 tokens)" $ length tokens > 100

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

    ((\(p, _, _) -> p) <$> (runParser emptyTokenMap parseTokens (unparse prog))) === Right prog

-- spec
spec :: Spec
spec = describe "Parser properties" $ do
    it "parse . unparse = id" $
        hedgehog $ do
            prop_parseUndoesUnparse