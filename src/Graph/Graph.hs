module Graph.Graph
  ( emptyGraph
  , graphFromList
  , insertVertex
  ) where

import Graph.Core
import qualified Data.Map.Strict as Map

emptyGraph :: Graph c v
emptyGraph = Graph Map.empty

insertVertex :: (Ord v) => v -> Graph c v -> Graph c v
insertVertex v g = g { graph = Map.insert v Map.empty $ graph g }

insertEdge :: (Ord v) => v -> v -> c -> Graph c v -> Graph c v
insertEdge outEdge inEdge cost g
  = g' { graph = Map.update (Just . Map.insert inEdge cost) outEdge $ graph g' }
  where g' = insertVertex outEdge $ insertVertex inEdge $ g

graphFromList :: (Ord v) => [(v, v, c)] -> Graph c v
graphFromList = foldr go emptyGraph
  where go (outEdge, inEdge, cost) = insertEdge outEdge inEdge cost
