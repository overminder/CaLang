module Backend.RegAlloc.Interference (
  Graph(..), VertexId,
  buildGraph,
  vertexWithMinimumDegree,
  removeVertex,

  graphToDot,
  rawGraphToDot,
) where

import Prelude hiding (concatMap, mapM_)
import Control.Monad.State hiding (mapM_, forM_)
import Data.Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Dot

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.RegAlloc.Liveness
import Backend.Operand
import Utils.Unique

type VertexId = Unique

data Graph
  = Graph {
    vertices    :: Set VertexId,
    edges       :: Map VertexId (Set VertexId),
    vertexToReg :: Map VertexId Reg,
    regToVertex :: Map Reg VertexId
  }

buildGraph :: Fg.FlowGraph (Liveness a) -> UniqueM Graph
buildGraph g = go
  where
    livenessInfo = map Set.toList (concatMap (foldMap combine) blockList)
    blockList = Map.elems (Fg.blockMap g)
    combine lv = [liveIn lv, liveOut lv]
    emptyGraph = Graph {
      vertices = Set.empty,
      edges = Map.empty,
      vertexToReg = Map.empty,
      regToVertex = Map.empty
    }

    go = flip execStateT emptyGraph $ do
      mapM_ addInterference livenessInfo

    addInterference regs = case regs of
      x:xs -> do
        vX <- addVertex x
        forM_ xs $ \y -> do
          vY <- addVertex y
          addEdge vX vY
        addInterference xs
      _ -> return ()

    addVertex r = do
      mbV <- gets (Map.lookup r . regToVertex)
      case mbV of
        Just v -> return v
        Nothing -> do
          v <- mkUnique
          modify $ \st -> st {
            vertices = Set.insert v (vertices st),
            edges = Map.insert v Set.empty (edges st),
            vertexToReg = Map.insert v r (vertexToReg st),
            regToVertex = Map.insert r v (regToVertex st)
          }
          return v

    addEdge v1 v2 = do
      modify $ \st -> st {
        edges = conn v1 v2 . conn v2 v1 . edges $ st
      }
      where
        conn a b = Map.adjust (Set.insert b) a

-- Helper functions

removeVertex :: VertexId -> Graph -> Graph
removeVertex v g = g {
  vertices = Set.delete v (vertices g),
  edges = removeThis . removeOthers $ (edges g)
  -- We intend to left reg<->vertex unchanged since they does not affect
  -- regalloc and it would be easiler to reconstruct the graph in this way.
}
  where
    removeThis mp = Map.delete v mp
    removeOthers mp = let others = mp Map.! v
                          combine v' mp = Map.adjust (Set.delete v) v' mp
                       in Set.foldr combine mp others

vertexWithMinimumDegree :: Graph -> VertexId
vertexWithMinimumDegree g = fst . minimumBy sorter $ edgeList
  where
    edgeList = Map.toList (edges g)
    sorter (_, v1) (_, v2) = compare (Set.size v1) (Set.size v2)

-- Dot support

graphToDot :: Graph -> (VertexId -> String) -> Dot ()
graphToDot (Graph vs es rs _) showVertex = (`evalStateT` Set.empty) $ do
  forM_ vs $ \v -> do
    let node = userNodeId v
    lift $ userNode node [("label", showVertex v),
                          ("shape", "circle"),
                          ("fontsize", "8.00")]
    forM_ (es Map.! v) $ \v' -> do
      let node' = userNodeId v'
      alreadyConnected <- gets (Set.member (v', v))
      if not alreadyConnected
        then do
          lift $ edge node node' [("dir", "none")]  -- undirected graph
          modify (Set.insert (v, v'))
        else return ()

rawGraphToDot g = graphToDot g showVertex
  where
    showVertex v = show $ pprReg ((vertexToReg g) Map.! v)

