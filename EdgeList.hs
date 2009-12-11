module Graph.EdgeList
( size
, nodes
, edges
, mirror
, adjacent
, indegree
, outdegree
) where

import Graph

data EdgeListGraph = EdgeListGraph [Node] [Edge] deriving (Show, Eq)

instance Graph EdgeListGraph where
    size      (EdgeListGraph ns _)      = length ns
    nodes     (EdgeListGraph ns _)      = ns
    edges     (EdgeListGraph _ es)      = es
    mirror    (EdgeListGraph ns es)     = EdgeListGraph ns (map (\(s, t) -> (t, s)) es)
    adjacent  (EdgeListGraph _ es) n    = map snd $ filter (\(s, t) -> s == n) es
    indegree  (EdgeListGraph _ es) n    = length $ filter((==) n . snd) es
    outdegree (EdgeListGraph _ es) n    = length $ filter((==) n . fst) es
    fromLists ns es                     = (EdgeListGraph ns es)

