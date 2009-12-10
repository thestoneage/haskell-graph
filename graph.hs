type Node = Int
type Edge = (Node, Node)

class Graph g where
    size     :: g -> Int
    nodes    :: g -> [Node]
    edges    :: g -> [Edge]
    reverse  :: g -> g
    adjacent :: g -> Node -> [Node]
    indegree :: g -> Node -> Int
    outdegree :: g -> Node -> Int

data EdgeListGraph = EdgeListGraph [Node] [Edge] deriving (Show, Eq)

instance Graph EdgeListGraph where
    size      (EdgeListGraph ns _)    = length ns
    nodes     (EdgeListGraph ns _)    = ns
    edges     (EdgeListGraph _ es)    = es
    reverse   (EdgeListGraph ns es)   = EdgeListGraph ns (map (\(s, t) -> (t, s)) es)
    adjacent  (EdgeListGraph _ es) n  = map snd $ filter (\(s, t) -> s == n) es
    indegree  (EdgeListGraph _ es) n  = length $ filter((==) n . snd) es
    outdegree (EdgeListGraph _ es) n  = length $ filter((==) n . fst) es
