module Backend.RegAlloc.IGraph (
  buildIGraph,
  IGraph(..), Vertex,
  minimumInterferenceNode,
  removeLiveRange,

  -- Generate dot
  iGraphToDot,
  rawIGraphToDot,
) where

-- Interference graph implementation using fgl

import Debug.Trace

import Prelude hiding (concatMap, mapM_, notElem)
import Control.Monad.State hiding (mapM_, forM_)
import qualified Data.Graph.Inductive as G
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Function
import Data.List hiding (concatMap, notElem)
import Text.Dot

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.RegAlloc.Liveness
import Backend.Operand
import Utils.Unique

type Vertex = G.Node

data IGraph
  = IGraph {
    liveGraph :: G.Gr () (),
    moveGraph :: G.Gr () (),
    vertex2Reg :: Map Vertex Reg,
    reg2Vertex :: Map Reg Vertex
  }

mLiveGraph f = \st -> st { liveGraph = f (liveGraph st) }
mMoveGraph f = \st -> st { moveGraph = f (moveGraph st) }
mV2R f = \st -> st { vertex2Reg = f (vertex2Reg st) }
mR2V f = \st -> st { reg2Vertex = f (reg2Vertex st) }

buildIGraph :: (Instruction a, MonadUnique m) =>
               Fg.FlowGraph (Liveness a) -> m IGraph
buildIGraph fg = go
  where
    blockList = Map.elems (Fg.blockMap fg)
    flattenedInstrs = concatMap toList blockList

    emptyIGraph = IGraph {
      liveGraph = G.empty,
      moveGraph = G.empty,
      vertex2Reg = Map.empty,
      reg2Vertex = Map.empty
    }

    go = flip execStateT emptyIGraph $ do
      forM_ flattenedInstrs $ \i -> do
        addLiveRange (toList (liveIn i))
        addLiveRange (toList (liveOut i))
        if isSimpleMoveInstr i
          then addMove i
          else return ()

    addLiveRange regs = case regs of
      x:ys -> do
        nX <- addNode x
        forM_ ys $ \y -> do
          nY <- addNode y
          g <- gets liveGraph
          if nY `notElem` G.pre g nX
            then modify (mLiveGraph (G.insEdges [(nX, nY, ()), (nY, nX, ())]))
            else return ()
        addLiveRange ys
      _ -> return ()

    addNode r = do
      mbN <- gets (Map.lookup r . reg2Vertex)
      case mbN of
        Just oldNode -> do
          return oldNode
        Nothing -> do
          newNode <- mkUnique
          modify (mLiveGraph (G.insNode (newNode, ())))
          modify (mMoveGraph (G.insNode (newNode, ())))
          modify (mR2V (Map.insert r newNode))
          modify (mV2R (Map.insert newNode r))
          return newNode

    addMove i = do
      let [dest] = toList (defs i)
          [src] = toList (uses i)
        -- physReg -> physReg moves is added as well, although we cannot
        -- coalesce them. Doesn't matter, since we ignore them in the
        -- coalescing step.
        --
        -- However, we don't add move for self moves.
      if dest /= src
        then do
          nDest <- addNode dest
          nSrc <- addNode src
          g <- gets liveGraph
          if nSrc `notElem` G.pre g nDest
            then modify (mMoveGraph (G.insEdge (nSrc, nDest, ())))
            else return ()
        else
          return ()

minimumInterferenceNode :: IGraph -> G.Node
minimumInterferenceNode g = case sortedDegrees of
  x:_ -> snd x
  where
    calDegree (preds, node, _, _) = (:) (length preds, node)
    compareDegree = compare `on` fst
    sortedDegrees = sortBy compareDegree (G.ufold calDegree [] (liveGraph g))

removeLiveRange n g = g'
  where
    g' = g {
      liveGraph = G.delNode n (liveGraph g)
    }

-- Dot support

iGraphToDot :: IGraph -> (Vertex -> String) -> Dot ()
iGraphToDot (IGraph lvGraph mvGraph n2r r2n) showVertex = do
  (`evalStateT` Set.empty) $ do
    forM_ (G.nodes lvGraph) $ \v -> do
      let node = userNodeId v
      lift $ userNode node [("label", showVertex v),
                            ("shape", "circle"),
                            ("fontsize", "8.00")]
      -- Draw interference edge. Since it's a undirected graph, we need
      -- to check for duplicate drawings.
      forM_ (G.suc lvGraph v) $ \v' -> do
        tryConn v v' [("dir", "none")] True

      -- Draw move edge. Directed and dotted.
      forM_ (G.suc mvGraph v) $ \v' -> do
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

rawIGraphToDot g = iGraphToDot g showVertex
  where
    showVertex v = show $ pprReg (vertex2Reg g Map.! v)

