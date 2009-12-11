module Graph
( Node
, Edge
, Graph
, size
, nodes
, edges
, mirror
, indegree
, outdegree
, adjacent
, fromLists
) where

type Node = Integer
type Edge = (Node, Node)

class Graph g where
    size      :: g -> Int
    nodes     :: g -> [Node]
    edges     :: g -> [Edge]
    mirror    :: g -> g
    adjacent  :: g -> Node -> [Node]
    indegree  :: g -> Node -> Int
    outdegree :: g -> Node -> Int
    fromLists :: [Node] -> [Edge] -> g

tns = [1, 2, 3]
tes = [(1,2),(2,3), (1,3)]