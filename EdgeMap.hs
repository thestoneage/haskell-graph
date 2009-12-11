module Graph.EdgeMap
( size
, nodes
, edges
, adjacent
, outdegree
, indegree
) where 

import Graph
import qualified Data.Map as Map

type EdgeMap = Map.Map Node [Edge]
data EdgeMapGraph = EdgeMapGraph EdgeMap deriving (Show, Eq)

instance Graph EdgeMapGraph where
    size (EdgeMapGraph m) =  Map.size m
    nodes (EdgeMapGraph m) = Map.keys m
    edges (EdgeMapGraph m) = concat (Map.elems m)
    adjacent (EdgeMapGraph m) n = map snd $ Map.findWithDefault [] n m
    outdegree (EdgeMapGraph m) n = length $ Map.findWithDefault [] n m
    fromLists ns es = EdgeMapGraph (Map.fromListWith (++) $ (nodeList ++ edgeList))
        where edgeList = map (\(s,t) -> (s,[(s,t)])) es
              nodeList = map (\n -> (n,[])) ns
