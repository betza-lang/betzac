{-# LANGUAGE LambdaCase #-}

module Betzac.Debug.Dot.Visitor (toDot) where

import Betzac.AST.Phases
import Betzac.AST.Types
import Betzac.Debug.Dot.Dot
import Betzac.Debug.PrettyPrint (Summarizable (summarize))

import Control.Monad.Trans.State
import Data.List.NonEmpty (toList)
import Data.Text (Text, pack)
import Prelude hiding (lines)

toDot :: BetzaProgram Ps -> Text
toDot prog =
    pack
        . unlines
        $ ["digraph {", metaNode]
            ++ evalState (concatMap snd <$> mapM qualfiedStmtNode prog) 0
            ++ ["}"]

qualfiedStmtNode :: QualifiedStmt Ps -> DotNode
qualfiedStmtNode = \case
    n@(Override d _) -> rootNode (summarize n) [directiveNode d]
    n@(Plain d _) -> rootNode (summarize n) [directiveNode d]

directiveNode :: Directive Ps -> DotNode
directiveNode = \case
    n@(Using f _) -> myNode n [filePathNode f]
    n@(Export s _) -> myNode n [stmtNode s]
    n@(Bare s _) -> myNode n [stmtNode s]
  where
    myNode = (midNode 2) . summarize

filePathNode :: FilePath -> DotNode
filePathNode = leafNode

stmtNode :: BetzaStmt Ps -> DotNode
stmtNode = \case
    n@(Assign l e _) -> myNode n [labelNode l, exprNode e]
    n@(LabelRef l _) -> myNode n [labelNode l]
  where
    myNode = (midNode 3) . summarize

exprNode :: BetzaExpr Ps -> DotNode
exprNode n@(BetzaExpr c _) = myNode n [chainExprNode c]
  where
    myNode = (midNode 5) . summarize

chainExprNode :: ChainExpr Ps -> DotNode
chainExprNode n@(ChainExpr u mcl _) =
    myNode n $ [unionExprNode u] <> maybe [] (\cl -> [chainLegNode cl]) mcl
  where
    myNode = (midNode 5) . summarize

chainLegNode :: ChainLeg Ps -> DotNode
chainLegNode n@(ChainLeg op c _) = myNode n [chainOperatorNode op, chainExprNode c]
  where
    myNode = (midNode 5) . summarize

unionExprNode :: UnionExpr Ps -> DotNode
unionExprNode n@(UnionExpr ms _) = myNode n $ map modifierExprNode (toList ms)
  where
    myNode = (midNode 6) . summarize

modifierExprNode :: ModifierExpr Ps -> DotNode
modifierExprNode n@(ModifierExpr _ ms a _) = myNode n $ map modifierNode ms ++ [exponentExprNode a]
  where
    myNode = (midNode 7) . summarize

modifierNode :: Modifier Ps -> DotNode
modifierNode = \case
    n@(Directional d _) -> myNode n [directionModifierNode d]
    n@(Behavioural b _) -> myNode n [behaviourNode b]
  where
    myNode = (midNode 7) . summarize

directionModifierNode :: DirectionModifier Ps -> DotNode
directionModifierNode = \case
    n@(Amalgamated d1 d2 _) -> myNode n [directionNode d1, directionNode d2]
    n@(Single d _) -> myNode n [directionNode d]
  where
    myNode = (midNode 8) . summarize

directionNode :: Direction Ps -> DotNode
directionNode = leafNode . summarize

behaviourNode :: Behaviour Ps -> DotNode
behaviourNode n@(Behaviour kind modality _) = myNode n [behaviourKindNode kind, behaviourModalityNode modality]
  where
    myNode = (midNode 8) . summarize

behaviourKindNode :: BehaviourKind Ps -> DotNode
behaviourKindNode = leafNode . summarize

behaviourModalityNode :: BehaviourModality Ps -> DotNode
behaviourModalityNode = leafNode . summarize

exponentExprNode :: ExponentExpr Ps -> DotNode
exponentExprNode n@(ExponentExpr a me _) = myNode n $ atomExprNode a : maybe [] (\e -> [exponentNode e]) me
  where
    myNode = (midNode 7) . summarize

atomExprNode :: AtomExpr Ps -> DotNode
atomExprNode = \case
    n@(Paren e _) -> myNode n [exprNode e]
    n@(From l _) -> myNode n [labelNode l]
  where
    myNode = (midNode 8) . summarize

exponentNode :: Exponent Ps -> DotNode
exponentNode n@(Exponent mco ms k _) =
    myNode n $
        maybe [] (\co -> [chainOperatorNode co]) mco
            <> map modifierNode ms
            <> [exponentKindNode k]
  where
    myNode = (midNode 8) . summarize

exponentKindNode :: ExponentKind Ps -> DotNode
exponentKindNode = leafNode . summarize

chainOperatorNode :: ChainOperator Ps -> DotNode
chainOperatorNode n@(ChainOperator k m _) = myNode n [chainKindNode k, chainModalityNode m]
  where
    myNode = (midNode 6) . summarize

chainKindNode :: ChainKind Ps -> DotNode
chainKindNode = leafNode . summarize

chainModalityNode :: ChainModality Ps -> DotNode
chainModalityNode = leafNode . summarize

labelNode :: Label Ps -> DotNode
labelNode = \case
    n@(Upper c _) -> myNode n [leafNode [c]]
    n@(Descriptor s _) -> myNode n [leafNode s]
    n@(Leaper x y _) -> myNode n [numberNode x, numberNode y]
  where
    myNode = (midNode 8) . summarize
