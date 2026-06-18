module Betzac.Debug.Dot.Dot (
    fresh,
    emit,
    edge,
    escape,
    child,
    children,
    node,
    DotNode,
    NodeId,
    DotM,
    metaNode,
    rootNode,
    leafNode,
    midNode,
    numberNode,
) where

import Control.Monad.Trans.State

type NodeId = Int
type DotM = State NodeId

fresh :: DotM NodeId
fresh = get <* modify (+ 1)

emit :: NodeId -> String -> String -> String -> DotM [String]
emit i label colour shape = return ["  " ++ show i ++ " [" ++ labelStr ++ " " ++ colorStr ++ " " ++ shapeStr ++ "]"]
  where
    labelStr = "label=\"" ++ label ++ "\""
    colorStr = "fillcolor=" ++ (show colour)
    shapeStr = "shape=" ++ shape

edge :: NodeId -> NodeId -> DotM [String]
edge from to = return ["  " ++ show from ++ " -> " ++ show to]

escape :: String -> String
escape = concatMap (\c -> if c == '"' then "\\\"" else [c])

type DotNode = DotM (NodeId, [String])

child :: NodeId -> DotNode -> DotM [String]
child parent mkChild = do
    (j, ls) <- mkChild
    es <- edge parent j
    return $ ls ++ es

children :: NodeId -> [DotNode] -> DotM [String]
children parent = fmap concat . mapM (child parent)

node :: String -> String -> String -> [DotNode] -> DotNode
node label colour shape subs = do
    i <- fresh
    ns <- emit i label colour shape
    cs <- children i subs
    return (i, ns ++ cs)

metaNode :: String
metaNode = "  node [fontname=monospace colorscheme=rdylgn9 style=filled]"

rootNode :: String -> [DotNode] -> DotNode
rootNode label = node label "1" "box"

midNode :: String -> Int -> [DotNode] -> DotNode
midNode label colour = node label (show colour) "box"

leafNode :: String -> DotNode
leafNode s = node (escape s) "#ccf0ff" "ellipse" []

numberNode :: Int -> DotNode
numberNode = leafNode . show
