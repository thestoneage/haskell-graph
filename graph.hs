import qualified Data.Map as Map

type Node = Integer
type Edge = (Node, Node)

class Graph g where
    size     :: g -> Int
    nodes    :: g -> [Node]
    edges    :: g -> [Edge]
    reverse  :: g -> g
    adjacent :: g -> Node -> [Node]
    indegree :: g -> Node -> Int
    outdegree :: g -> Node -> Int
    fromLists  :: g -> [Node] -> [Edge] -> g

data EdgeListGraph = EdgeListGraph [Node] [Edge] | EmptyEdgeListGraph deriving (Show, Eq)

instance Graph EdgeListGraph where
    size      (EdgeListGraph ns _)      = length ns
    nodes     (EdgeListGraph ns _)      = ns
    edges     (EdgeListGraph _ es)      = es
    reverse   (EdgeListGraph ns es)     = EdgeListGraph ns (map (\(s, t) -> (t, s)) es)
    adjacent  (EdgeListGraph _ es) n    = map snd $ filter (\(s, t) -> s == n) es
    indegree  (EdgeListGraph _ es) n    = length $ filter((==) n . snd) es
    outdegree (EdgeListGraph _ es) n    = length $ filter((==) n . fst) es
    fromLists  (EmptyEdgeListGraph) ns es = (EdgeListGraph ns es)

type EdgeMap = Map.Map Node [Edge]
data EdgeMapGraph = EdgeMapGraph EdgeMap | EmptyEdgeMapGraph deriving (Show, Eq)

instance Graph EdgeMapGraph where
    size (EdgeMapGraph m) =  Map.size m
    nodes (EdgeMapGraph m) = Map.keys m
    edges (EdgeMapGraph m) = concat (Map.elems m)
    adjacent (EdgeMapGraph m) n = map snd $ Map.findWithDefault [] n m
    outdegree (EdgeMapGraph m) n = length $ Map.findWithDefault [] n m
    indegree  (EdgeMapGraph m) n = length $ edges m
    fromLists (EmptyEdgeMapGraph) ns es = EdgeMapGraph (Map.fromListWith (++) $ (nodeList ++ edgeList))
        where edgeList = map (\(s,t) -> (s,[(s,t)])) es
              nodeList = map (\n -> (n,[])) ns
                            

tns = [1, 2, 3]
tes = [(1,2),(2,3), (1,3)]