{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Betzac.Debug.Dot.Visitor (toDot, Drawable) where

import Betzac.AST.Origin (HasOrigin (..), Origin (..))
import Betzac.AST.Phases
import Betzac.AST.Types
import Betzac.Debug.Dot.Dot
import Betzac.Debug.PrettyPrint (Summarizable (summarize))

import Control.Monad.Trans.State
import Data.List.NonEmpty (toList)
import Data.Text (Text, pack)
import Prelude hiding (lines)

-- | What a phase must offer to be drawn: a readable modifier string and an origin.
type Drawable p = (Qualifying p, OriginX p)

toDot :: (Drawable p) => BetzaProgram p -> Text
toDot prog =
    pack
        . unlines
        $ ["digraph {", metaNode]
            ++ evalState (concatMap snd <$> mapM qualfiedStmtNode prog) 0
            ++ ["}"]

-- Colours for what the source did not say. A written node keeps its structural colour.
impliedColour, restatedColour, contradictedColour :: String
impliedColour = "#dcc6f5"
restatedColour = "#ffd9a0"
contradictedColour = "#f2a0a0"

-- | The structural colour a node would have had, unless its origin overrides it.
tinted :: Origin -> Int -> String
tinted Written structural = show structural
tinted Implied _ = impliedColour
tinted Restated _ = restatedColour
tinted Contradicted _ = contradictedColour

originNode :: Origin -> Int -> String -> [DotNode] -> DotNode
originNode o structural label subs = node label (tinted o structural) "box" subs

originLeaf :: Origin -> String -> DotNode
originLeaf Written s = leafNode s
originLeaf o s = node (escape s) (tinted o 0) "ellipse" []

{- | An implied parent has implied children, but a restatement marks only the node
that restates: the letter beneath it really was written.
-}
inherit :: Origin -> Origin -> Origin
inherit Implied Written = Implied
inherit _ own = own

qualfiedStmtNode :: (Drawable p) => QualifiedStmt p -> DotNode
qualfiedStmtNode = \case
    n@(Override d _) -> rootNode (summarize n) [directiveNode d]
    n@(Plain d _) -> rootNode (summarize n) [directiveNode d]

directiveNode :: (Drawable p) => Directive p -> DotNode
directiveNode = \case
    n@(Using f _) -> myNode n [filePathNode f]
    n@(Export s _) -> myNode n [stmtNode s]
    n@(Bare s _) -> myNode n [stmtNode s]
  where
    myNode = (midNode 2) . summarize

filePathNode :: FilePath -> DotNode
filePathNode = leafNode

stmtNode :: (Drawable p) => BetzaStmt p -> DotNode
stmtNode = \case
    n@(Assign l e _) -> myNode n [labelNode l, exprNode e]
    n@(LabelRef l _) -> myNode n [labelNode l]
  where
    myNode = (midNode 3) . summarize

exprNode :: (Drawable p) => BetzaExpr p -> DotNode
exprNode n@(BetzaExpr c _) = myNode n [chainExprNode c]
  where
    myNode = (midNode 5) . summarize

chainExprNode :: (Drawable p) => ChainExpr p -> DotNode
chainExprNode n@(ChainExpr u mcl _) =
    myNode n $ [unionExprNode u] <> maybe [] (\cl -> [chainLegNode cl]) mcl
  where
    myNode = (midNode 5) . summarize

chainLegNode :: (Drawable p) => ChainLeg p -> DotNode
chainLegNode n@(ChainLeg op c _) = myNode n [chainOperatorNode op, chainExprNode c]
  where
    myNode = (midNode 5) . summarize

unionExprNode :: (Drawable p) => UnionExpr p -> DotNode
unionExprNode n@(UnionExpr ms _) = myNode n $ map modifierExprNode (toList ms)
  where
    myNode = (midNode 6) . summarize

modifierExprNode :: forall p. (Drawable p) => ModifierExpr p -> DotNode
modifierExprNode n@(ModifierExpr _ ms a _) =
    myNode n $ map modifierNode (toModifiers @p ms) ++ [exponentExprNode a]
  where
    myNode = (midNode 7) . summarize

modifierNode :: (Drawable p) => Modifier p -> DotNode
modifierNode = \case
    n@(Directional d _) -> myNode n [directionModifierNode d]
    n@(Behavioural b _) -> myNode n [behaviourNode b]
  where
    myNode = (midNode 7) . summarize

directionModifierNode :: (Drawable p) => DirectionModifier p -> DotNode
directionModifierNode n = case n of
    Amalgamated d1 d2 _ -> myNode [directionNode o d1, directionNode o d2]
    Single d _ -> myNode [directionNode o d]
  where
    o = origin n
    myNode = originNode o 8 (summarize n)

directionNode :: (Drawable p) => Origin -> Direction p -> DotNode
directionNode parent n = originLeaf (inherit parent (origin n)) (summarize n)

behaviourNode :: (Drawable p) => Behaviour p -> DotNode
behaviourNode n@(Behaviour kind modality _) =
    originNode o 8 (summarize n) [behaviourKindNode o kind, behaviourModalityNode o modality]
  where
    o = origin n

behaviourKindNode :: (Drawable p) => Origin -> BehaviourKind p -> DotNode
behaviourKindNode parent n = originLeaf (inherit parent (origin n)) (summarize n)

behaviourModalityNode :: (Drawable p) => Origin -> BehaviourModality p -> DotNode
behaviourModalityNode parent n = originLeaf (inherit parent (origin n)) (summarize n)

exponentExprNode :: (Drawable p) => ExponentExpr p -> DotNode
exponentExprNode n@(ExponentExpr a me _) = myNode n $ atomExprNode a : maybe [] (\e -> [exponentNode e]) me
  where
    myNode = (midNode 7) . summarize

atomExprNode :: (Drawable p) => AtomExpr p -> DotNode
atomExprNode = \case
    n@(Paren e _) -> myNode n [exprNode e]
    n@(From l _) -> myNode n [labelNode l]
  where
    myNode = (midNode 8) . summarize

exponentNode :: forall p. (Drawable p) => Exponent p -> DotNode
exponentNode n@(Exponent mco ms k _) =
    myNode n $
        maybe [] (\co -> [chainOperatorNode co]) (toJoint @p mco)
            <> map modifierNode (toModifiers @p ms)
            <> [exponentKindNode k]
  where
    myNode = (midNode 8) . summarize

exponentKindNode :: ExponentKind p -> DotNode
exponentKindNode = \case
    n@(Repeat x _) -> midNode 8 (summarize n) [numberNode x]
    n -> leafNode (summarize n)

chainOperatorNode :: (Drawable p) => ChainOperator p -> DotNode
chainOperatorNode n@(ChainOperator k m _) =
    originNode o 6 (summarize n) [chainKindNode o k, chainModalityNode o m]
  where
    o = origin n

chainKindNode :: (Drawable p) => Origin -> ChainKind p -> DotNode
chainKindNode parent n = originLeaf (inherit parent (origin n)) (summarize n)

chainModalityNode :: (Drawable p) => Origin -> ChainModality p -> DotNode
chainModalityNode parent n = originLeaf (inherit parent (origin n)) (summarize n)

labelNode :: Label p -> DotNode
labelNode = \case
    n@(Upper c _) -> myNode n [leafNode [c]]
    n@(Descriptor s _) -> myNode n [leafNode s]
    n@(Leaper x y _) -> myNode n [numberNode x, numberNode y]
  where
    myNode = (midNode 8) . summarize
