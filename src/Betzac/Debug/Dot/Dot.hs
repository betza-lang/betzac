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
    leaf,
    numberNode,
) where

import Control.Monad.Trans.State

type NodeId = Int
type DotM = State NodeId

fresh :: DotM NodeId
fresh = get <* modify (+ 1)

emit :: NodeId -> String -> DotM [String]
emit i label = return ["  " ++ show i ++ " [label=\"" ++ label ++ "\"]"]

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

node :: String -> [DotNode] -> DotNode
node label subs = do
    i <- fresh
    ns <- emit i label
    cs <- children i subs
    return (i, ns ++ cs)

leaf :: String -> DotNode
leaf s = node (escape s) []

numberNode :: Int -> DotNode
numberNode = leaf . show
