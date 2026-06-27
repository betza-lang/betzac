{-# LANGUAGE LambdaCase #-}

module Betzac.Debug.Dot.Visitor (toDot) where

import Betzac.AST
import Betzac.Debug.Dot.Dot
import Control.Monad.Trans.State
import Data.List.NonEmpty (toList)
import Data.Text (Text, pack)
import Prelude hiding (lines)

toDot :: BetzaProgram -> Text
toDot prog =
    pack
        . unlines
        $ ["digraph {", metaNode]
            ++ evalState (concatMap snd <$> mapM qualfiedStmtNode prog) 0
            ++ ["}"]

qualfiedStmtNode :: QualifiedStmt -> DotNode
qualfiedStmtNode = \case
    Override d -> rootNode "Override :: QualifiedStmt" [directiveNode d]
    Plain d -> rootNode "Plain :: QualifiedStmt" [directiveNode d]

directiveNode :: Directive -> DotNode
directiveNode = \case
    Using f -> myNode "Using :: Directive" [filePathNode f]
    Export s -> myNode "Export :: Directive" [stmtNode s]
    Bare s -> myNode "Bare :: Directive" [stmtNode s]
  where
    myNode l = midNode l 2

filePathNode :: FilePath -> DotNode
filePathNode = leafNode

stmtNode :: BetzaStmt -> DotNode
stmtNode = \case
    Assign l e -> myNode "Assign :: BetzaStmt" [labelNode l, exprNode e]
    Anonymous e -> myNode "Anonymous :: BetzaStmt" [exprNode e]
  where
    myNode l = midNode l 3

exprNode :: BetzaExpr -> DotNode
exprNode (BetzaExpr c) = myNode "BetzaExpr" [chainExprNode c]
  where
    myNode l = midNode l 5

chainExprNode :: ChainExpr -> DotNode
chainExprNode (ChainExpr u mcl) =
    myNode "ChainExpr" $
        [unionExprNode u]
            <> maybe [] (\cl -> [chainLegNode cl]) mcl
  where
    myNode l = midNode l 5

chainLegNode :: ChainLeg -> DotNode
chainLegNode (ChainLeg op c) = myNode "ChainLeg" [chainOperatorNode op, chainExprNode c]
  where
    myNode l = midNode l 5

unionExprNode :: UnionExpr -> DotNode
unionExprNode (UnionExpr ms) = myNode "UnionExpr" $ map modifierExprNode (toList ms)
  where
    myNode l = midNode l 6

modifierExprNode :: ModifierExpr -> DotNode
modifierExprNode (ModifierExpr s ms a) =
    myNode ("ModifierExpr -- setup=" ++ show s) $
        map modifierNode ms ++ [exponentExprNode a]
  where
    myNode l = midNode l 7

modifierNode :: Modifier -> DotNode
modifierNode = \case
    Directional d -> myNode "Directional :: Modifier" [directionModifierNode d]
    Behavioural b -> myNode "Behavioural :: Modifier" [behaviourNode b]
  where
    myNode l = midNode l 7

directionModifierNode :: DirectionModifier -> DotNode
directionModifierNode = \case
    Amalgamated d1 d2 -> myNode "Amalgamated :: DirectionModifier" [directionNode d1, directionNode d2]
    Single d -> myNode "Single :: DirectionModifier" [directionNode d]
  where
    myNode l = midNode l 8

directionNode :: Direction -> DotNode
directionNode = leafNode . show

behaviourNode :: Behaviour -> DotNode
behaviourNode (Behaviour kind modality) = myNode "Behaviour" [behaviourKindNode kind, behaviourModalityNode modality]
  where
    myNode l = midNode l 8

behaviourKindNode :: BehaviourKind -> DotNode
behaviourKindNode = leafNode . show

behaviourModalityNode :: BehaviourModality -> DotNode
behaviourModalityNode = leafNode . show

exponentExprNode :: ExponentExpr -> DotNode
exponentExprNode (ExponentExpr a me) =
    myNode "ExponentExpr" $
        atomExprNode a : maybe [] (\e -> [exponentNode e]) me
  where
    myNode l = midNode l 7

atomExprNode :: AtomExpr -> DotNode
atomExprNode = \case
    Paren e -> myNode "Paren :: AtomExpr" [exprNode e]
    From l -> myNode "From :: AtomExpr" [labelNode l]
  where
    myNode l = midNode l 8

exponentNode :: Exponent -> DotNode
exponentNode (Exponent mco ms k) =
    myNode "Exponent" $
        maybe [] (\co -> [chainOperatorNode co]) mco
            <> map modifierNode ms
            <> [exponentKindNode k]
  where
    myNode l = midNode l 8

exponentKindNode :: ExponentKind -> DotNode
exponentKindNode = leafNode . show

chainOperatorNode :: ChainOperator -> DotNode
chainOperatorNode (ChainOperator k m) = myNode "ChainOperator" [chainKindNode k, chainModalityNode m]
  where
    myNode l = midNode l 6

chainKindNode :: ChainKind -> DotNode
chainKindNode = leafNode . show

chainModalityNode :: ChainModality -> DotNode
chainModalityNode = leafNode . show

labelNode :: Label -> DotNode
labelNode = \case
    Upper c -> myNode "Upper :: Label" [leafNode [c]]
    Descriptor s -> myNode "Descriptor :: Label" [leafNode s]
    Leaper m n -> myNode "Leaper :: Label" [numberNode m, numberNode n]
  where
    myNode l = midNode l 8
