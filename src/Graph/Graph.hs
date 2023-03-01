module Graph.Graph
  ( emptyGraph
  , graphFromEdgeList
  , graphFromAdjacencyList
  , insertVertex
  , mapVertices
  , mapCosts
  ) where

import Graph.Core
import qualified Data.Map.Strict as Map

emptyGraph :: Graph c v
emptyGraph = Graph Map.empty

insertVertex :: (Ord v) => v -> Graph c v -> Graph c v
insertVertex v = Graph . Map.insert v Map.empty . graph

insertEdge :: (Ord v) => v -> v -> c -> Graph c v -> Graph c v
insertEdge outVertex inVertex cost
  = Graph
  . Map.update (Just . Map.insert inVertex cost) outVertex
  . graph
  . insertVertex outVertex
  . insertVertex inVertex

graphFromEdgeList :: (Ord v) => [(v, v, c)] -> Graph c v
graphFromEdgeList = foldr go emptyGraph
  where go (outVertex, inVertex, cost) = insertEdge outVertex inVertex cost

graphFromAdjacencyList :: (Ord v) => [(v, [(v, c)])] -> Graph c v
graphFromAdjacencyList = Graph . foldr go Map.empty
  where go (v, adjacents) = Map.insert v (Map.fromList adjacents)

mapVertices :: (Ord v') => (v -> v') -> Graph c v -> Graph c v'
mapVertices f 
  = Graph
  . Map.mapKeys f
  . Map.map (Map.mapKeys f)
  . graph

mapCosts :: (c -> c') -> Graph c v -> Graph c' v
mapCosts f = Graph
  . Map.map (Map.map f)
  . graph
