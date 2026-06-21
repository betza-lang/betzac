module Parser.ParserHedgehog (spec) where

import Betzac.AST
import Betzac.Parser.Core (runParser)
import Betzac.Parser.Parser (parseTokens)
import Betzac.Token

import Betzac.Lexer.ErrorHandling (emptyTokenMap)
import Lexer.LexerQC (unlex)

import Data.List (intercalate, singleton)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)

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
unparseDirective (Using f) = [TokUsing f]
unparseDirective (Export s) = TokExport : unparseStmt s
unparseDirective (Bare s) = unparseStmt s

unparseStmt :: BetzaStmt -> [Token]
unparseStmt (Assign l e) = unparseLabel l : unparseExpr e
unparseStmt (Alias a l) = [unparseLabel a, unparseLabel l]
unparseStmt (Resolve l) = [unparseLabel l]
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
unparseChainTail (co : t) =
    [unparseChainOp (fst co)]
        <> unparseOption (snd co)
        <> unparseChainTail t
unparseChainTail [] = []

unparseChainOp :: ChainOperator -> Token
unparseChainOp Step = TokChainStep
unparseChainOp Sequence = TokChainSequence

unparseOption :: OptionExpr -> [Token]
unparseOption (Choose e) = [TokLBrace] <> unparseExpr e <> [TokRBrace]
unparseOption (IffUnblocked e) = [TokLBracket] <> unparseExpr e <> [TokRBracket]
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
genModifiers = Gen.sized (\(Size n) -> Gen.list (Range.linear 0 (max 1 n)) genModifier)

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
genUnion = UnionExpr <$> Gen.sized (\(Size n) -> Gen.nonEmpty (Range.linear 1 (max 1 n)) genModifierExpr)

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
genChainTail = Gen.sized $ \(Size n) -> Gen.list (Range.linear 0 n) ((,) <$> genChainOperator <*> genOption)

genExpr :: Gen BetzaExpr
genExpr = BetzaExpr <$> genChain

genStmt :: Gen BetzaStmt
genStmt =
    Gen.choice
        [ Anonymous <$> genNonTrivialExpr
        , Resolve <$> genLabel
        , Alias <$> genLabel <*> genLabel
        , Assign <$> genLabel <*> genExpr
        ]

-- To prevent the degenerate and ambiguous case of Resolve (Upper c) and the bare label expression below unparsing to the same thing
genNonTrivialExpr :: Gen BetzaExpr
genNonTrivialExpr = Gen.filter (not . isBareLabel) genExpr
  where
    isBareLabel (BetzaExpr (ChainExpr (Mandatory (UnionExpr (ModifierExpr False [] (ExponentExpr (From (Upper _)) Nothing) :| []))) [])) = True
    isBareLabel _ = False

genFilePath :: Gen FilePath
genFilePath =
    intercalate "."
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

-- props

prop_parseUndoesUnparse :: PropertyT IO ()
prop_parseUndoesUnparse = do
    prog <- forAll genProgram
    annotate . unlex . unparse $ prog
    ((\(p, _, _) -> p) <$> (runParser emptyTokenMap parseTokens (unparse prog))) === Right prog

-- spec
spec :: Spec
spec = describe "Parser properties" $ do
    it "parse . unparse = id" $
        hedgehog $ do
            prop_parseUndoesUnparse