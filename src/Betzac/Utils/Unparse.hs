{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Betzac.Utils.Unparse (
    unparse,
    unparseQualifiedStmt,
    unparseDirective,
    unparseFilePath,
    unparseStmt,
    unparseLabel,
    unparseExpr,
    unparseChain,
    unparseChainLeg,
    unparseChainOp,
    unparseChainKind,
    unparseUnion,
    unparseModExpr,
    unparseModifier,
    unparseDirectionMod,
    unparseDirection,
    unparseBehaviour,
    unparseBehaviourKind,
    unparseExponentExpr,
    unparseAtom,
    unparseExponent,
    unparseExponentKind,
) where

import Betzac.AST
import qualified Betzac.Token as B

unparse :: (Qualifying a) => BetzaProgram a -> [B.Token]
unparse stmts = concat $ unparseQualifiedStmt <$> stmts

unparseQualifiedStmt :: (Qualifying a) => QualifiedStmt a -> [B.Token]
unparseQualifiedStmt (Override d _) = B.TokOverride : unparseDirective d <> [B.TokEndStmt]
unparseQualifiedStmt (Plain d _) = unparseDirective d <> [B.TokEndStmt]

unparseDirective :: (Qualifying a) => Directive a -> [B.Token]
unparseDirective (Using f _) = [B.TokUsing $ unparseFilePath f]
unparseDirective (Export s _) = B.TokExport : unparseStmt s
unparseDirective (Bare s _) = unparseStmt s

unparseFilePath :: FilePath -> FilePath
unparseFilePath f = (\c -> if c == '/' then '.' else c) <$> f

unparseStmt :: (Qualifying a) => BetzaStmt a -> [B.Token]
unparseStmt (Assign l e _) = unparseLabel l : B.TokAssign : unparseExpr e
unparseStmt (LabelRef l _) = [unparseLabel l]

unparseLabel :: Label a -> B.Token
unparseLabel (Upper c _) = B.TokAtom c
unparseLabel (Descriptor s _) = B.TokDescriptor s
unparseLabel (Leaper a b _) = B.TokDescriptor $ show a ++ "," ++ show b

unparseExpr :: (Qualifying a) => BetzaExpr a -> [B.Token]
unparseExpr (BetzaExpr c _) = unparseChain c

unparseChain :: (Qualifying a) => ChainExpr a -> [B.Token]
unparseChain (ChainExpr u mcl _) = unparseUnion u <> maybe [] unparseChainLeg mcl

unparseChainLeg :: (Qualifying a) => ChainLeg a -> [B.Token]
unparseChainLeg (ChainLeg op c _) = unparseChainOp op c

unparseChainOp :: (Qualifying a) => ChainOperator a -> ChainExpr a -> [B.Token]
unparseChainOp (ChainOperator k (Mandatory _) _) c =
    unparseChainKind k : unparseChain c
unparseChainOp (ChainOperator k (IffUnblocked _) _) c =
    [unparseChainKind k, B.TokLBrace] <> unparseChain c <> [B.TokRBrace]
unparseChainOp (ChainOperator k (Choose _) _) c =
    [unparseChainKind k, B.TokLBracket] <> unparseChain c <> [B.TokRBracket]

unparseChainKind :: ChainKind a -> B.Token
unparseChainKind (Step _) = B.TokChainStep
unparseChainKind (Sequence _) = B.TokChainSequence

unparseUnion :: (Qualifying a) => UnionExpr a -> [B.Token]
unparseUnion (UnionExpr ms _) = concat $ unparseModExpr <$> ms

unparseModExpr :: forall a. (Qualifying a) => ModifierExpr a -> [B.Token]
unparseModExpr (ModifierExpr s ms e _) =
    (if s then [B.TokBang] else [])
        <> (toModifiers @a ms >>= unparseModifier)
        <> unparseExponentExpr e

unparseModifier :: Modifier a -> [B.Token]
unparseModifier (Directional m _) = unparseDirectionMod m
unparseModifier (Behavioural m _) = unparseBehaviour m

unparseDirectionMod :: DirectionModifier a -> [B.Token]
unparseDirectionMod (Amalgamated d1 d2 _) =
    [ B.TokLAngle
    , unparseDirection d1
    , unparseDirection d2
    , B.TokRAngle
    ]
unparseDirectionMod (Single d _) = [unparseDirection d]

unparseDirection :: Direction a -> B.Token
unparseDirection (Forward _) = B.TokDirection 'f'
unparseDirection (Backward _) = B.TokDirection 'b'
unparseDirection (Leftward _) = B.TokDirection 'l'
unparseDirection (Rightward _) = B.TokDirection 'r'
unparseDirection (Sideway _) = B.TokDirection 's'
unparseDirection (Vertically _) = B.TokDirection 'v'
unparseDirection (All _) = B.TokDirection 'a'

unparseBehaviour :: Behaviour a -> [B.Token]
unparseBehaviour (Behaviour kind (Once _) _) = [unparseBehaviourKind kind]
unparseBehaviour (Behaviour kind (Twice _) _) = [unparseBehaviourKind kind, unparseBehaviourKind kind]
unparseBehaviour (Behaviour kind (Any _) _) = [unparseBehaviourKind kind, B.TokBehaviour 'y']

unparseBehaviourKind :: BehaviourKind a -> B.Token
unparseBehaviourKind (Capture _) = B.TokBehaviour 'c'
unparseBehaviourKind (Leap _) = B.TokBehaviour 'g'
unparseBehaviourKind (Initial _) = B.TokBehaviour 'i'
unparseBehaviourKind (Jump _) = B.TokBehaviour 'j'
unparseBehaviourKind (Move _) = B.TokBehaviour 'm'
unparseBehaviourKind (NoJump _) = B.TokBehaviour 'n'
unparseBehaviourKind (Hop _) = B.TokBehaviour 'p'

unparseExponentExpr :: (Qualifying a) => ExponentExpr a -> [B.Token]
unparseExponentExpr (ExponentExpr a me _) = unparseAtom a <> maybe [] unparseExponent me

unparseAtom :: (Qualifying a) => AtomExpr a -> [B.Token]
unparseAtom (Paren e _) = [B.TokLParen] <> unparseExpr e <> [B.TokRParen]
unparseAtom (From l _) = [unparseLabel l]

unparseExponent :: forall a. (Qualifying a) => Exponent a -> [B.Token]
unparseExponent (Exponent mop ms' k _) = case toJoint @a mop of
    Nothing -> (toModifiers @a ms' >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck (Mandatory _) _) -> [unparseChainKind ck] <> (toModifiers @a ms' >>= unparseModifier) <> [unparseExponentKind k]
    Just (ChainOperator ck (IffUnblocked _) _) -> [unparseChainKind ck, B.TokLBrace] <> (toModifiers @a ms' >>= unparseModifier) <> [unparseExponentKind k, B.TokRBrace]
    Just (ChainOperator ck (Choose _) _) -> [unparseChainKind ck, B.TokLBracket] <> (toModifiers @a ms' >>= unparseModifier) <> [unparseExponentKind k, B.TokRBracket]

unparseExponentKind :: ExponentKind a -> B.Token
unparseExponentKind (Infinite _) = B.TokNumber 0
unparseExponentKind (Slippery _) = B.TokSlippery
unparseExponentKind (Repeat n _) = B.TokNumber n
