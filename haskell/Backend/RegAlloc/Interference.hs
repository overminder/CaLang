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
    moveEdges   :: Map VertexId (Set VertexId),
    vertexToReg :: Map VertexId Reg,
    regToVertex :: Map Reg VertexId
  }

buildGraph :: Instruction a => Fg.FlowGraph (Liveness a) -> UniqueM Graph
buildGraph g = go
  where
    blockList = Map.elems (Fg.blockMap g)
    flattenedInstrs = concatMap toList blockList

    emptyGraph = Graph {
      vertices = Set.empty,
      edges = Map.empty,
      moveEdges = Map.empty,
      vertexToReg = Map.empty,
      regToVertex = Map.empty
    }

    go = flip execStateT emptyGraph $ do
      forM_ flattenedInstrs $ \i -> do
        addInterference (toList (liveIn i))
        addInterference (toList (liveOut i))
        if isSimpleMoveInstr i
          then addMove i
          else return ()

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
            moveEdges = Map.insert v Set.empty (moveEdges st),
            vertexToReg = Map.insert v r (vertexToReg st),
            regToVertex = Map.insert r v (regToVertex st)
          }
          return v

    addEdge v1 v2 = do
      modify $ \st -> st {
        edges = conn v1 v2 . conn v2 v1 . edges $ st
      }

    addMove i = do
      let [dest] = toList (defs i)
          [src] = toList (uses i)
      if isVirtualReg dest || isVirtualReg src
        then do
          vDest <- addVertex dest
          vSrc <- addVertex src
          addMoveEdge vDest vSrc
        else
          -- We ignore physReg -> physReg moves,
          -- since we cannot coalesce them.
          return ()

    addMoveEdge vDest vSrc = do
      modify $ \st -> st {
        moveEdges = conn vDest vSrc (moveEdges st)
      }

    conn dest src = Map.adjust (Set.insert dest) src

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
graphToDot (Graph vs es mes rs _) showVertex = (`evalStateT` Set.empty) $ do
  forM_ vs $ \v -> do
    let node = userNodeId v
    lift $ userNode node [("label", showVertex v),
                          ("shape", "circle"),
                          ("fontsize", "8.00")]
    -- Draw interference edge. Since it's a undirected graph, we need
    -- to check for duplicate drawings.
    forM_ (es Map.! v) $ \v' -> do
      tryConn v v' [("dir", "none")] True

    -- Draw move edge. Directed and dotted.
    forM_ (Map.findWithDefault Set.empty v mes) $ \v' -> do
      tryConn v v' [("style", "dotted")] False
  where
    tryConn v1 v2 style checkDup = do
      alreadyConnected <- gets (Set.member (v2, v1))
      if not checkDup || not alreadyConnected
        then do
          lift $ edge (userNodeId v1) (userNodeId v2) style
          modify (Set.insert (v1, v2))
        else
          return ()

rawGraphToDot g = graphToDot g showVertex
  where
    showVertex v = show $ pprReg ((vertexToReg g) Map.! v)

