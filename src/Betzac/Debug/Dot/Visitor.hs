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
        $ ["digraph {", "  node [shape=box fontname=monospace]"]
            ++ evalState (concatMap snd <$> mapM qualfiedStmtNode prog) 0
            ++ ["}"]

qualfiedStmtNode :: QualifiedStmt -> DotNode
qualfiedStmtNode = \case
    Override d -> node "Override :: QualifiedStmt" [directiveNode d]
    Plain d -> node "Plain :: QualifiedStmt" [directiveNode d]

directiveNode :: Directive -> DotNode
directiveNode = \case
    Using f -> node "Using :: Directive" [filePathNode f]
    Export s -> node "Export :: Directive" [stmtNode s]
    Bare s -> node "Bare :: Directive" [stmtNode s]

filePathNode :: FilePath -> DotNode
filePathNode f = node (escape f) []

stmtNode :: BetzaStmt -> DotNode
stmtNode = \case
    Assign l e -> node "Assign :: BetzaStmt" [labelNode l, exprNode e]
    Alias a l -> node "Alias :: BetzaStmt" [labelNode a, labelNode l]
    Resolve l -> node "Resolve :: BetzaStmt" [labelNode l]
    Anonymous e -> node "Anonymous :: BetzaStmt" [exprNode e]

exprNode :: BetzaExpr -> DotNode
exprNode (BetzaExpr c) = node "BetzaExpr" [chainExprNode c]

chainExprNode :: ChainExpr -> DotNode
chainExprNode (ChainExpr o rest) = node "ChainExpr" $ optionExprNode o : map chainLinkNode rest

chainLinkNode :: (ChainOperator, OptionExpr) -> DotNode
chainLinkNode (op, e) =
    node (show op) [optionExprNode e]

optionExprNode :: OptionExpr -> DotNode
optionExprNode = \case
    Choose e -> node "Choose :: OptionExpr" [exprNode e]
    IffUnblocked e -> node "IffUnblocked :: OptionExpr" [exprNode e]
    Mandatory u -> node "Mandatory :: OptionExpr" [unionExprNode u]

unionExprNode :: UnionExpr -> DotNode
unionExprNode (UnionExpr ms) = node "UnionExpr" $ map modifierExprNode (toList ms)

modifierExprNode :: ModifierExpr -> DotNode
modifierExprNode (ModifierExpr s ms a) =
    node ("ModifierExpr -- setup=" ++ show s) $
        map modifierNode ms ++ [exponentExprNode a]

modifierNode :: Modifier -> DotNode
modifierNode = \case
    Directional d -> node "Directional :: Modifier" [directionModifierNode d]
    Behavioural b -> node "Behavioural :: Modifier" [behaviourNode b]

directionModifierNode :: DirectionModifier -> DotNode
directionModifierNode = \case
    Amalgamated d1 d2 -> node "Amalgamated :: DirectionModifier" [directionNode d1, directionNode d2]
    Single d -> node "Single :: DirectionModifier" [directionNode d]

directionNode :: Direction -> DotNode
directionNode d =
    node (show d) []

behaviourNode :: Behaviour -> DotNode
behaviourNode b =
    node (show b) []

exponentExprNode :: ExponentExpr -> DotNode
exponentExprNode (ExponentExpr a me) =
    node "ExponentExpr" $ atomExprNode a : maybe [] (\e -> [exponentNode e]) me

atomExprNode :: AtomExpr -> DotNode
atomExprNode = \case
    Paren e -> node "Paren :: AtomExpr" [exprNode e]
    From l -> node "From :: AtomExpr" [labelNode l]

exponentNode :: Exponent -> DotNode
exponentNode = \case
    Infinite -> node "Infinite :: Exponent" []
    Repeat mco ms n ->
        node "Repeat :: Exponent" $
            maybe [] (\co -> [chainOperatorNode co]) mco
                ++ map modifierNode ms
                ++ [numberNode n]

chainOperatorNode :: ChainOperator -> DotNode
chainOperatorNode op = node (show op) []

labelNode :: Label -> DotNode
labelNode = \case
    Upper c -> node "Upper :: Label" [leaf [c]]
    Descriptor s -> node "Descriptor :: Label" [leaf s]
    Leaper m n -> node "Leaper :: Label" [numberNode m, numberNode n]
