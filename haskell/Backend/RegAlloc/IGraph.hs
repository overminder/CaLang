module Backend.RegAlloc.IGraph (
) where

-- Interference graph implementation using fgl

import Prelude hiding (concatMap, mapM_)
import Control.Monad.State hiding (mapM_, forM_)
import qualified Data.Graph.Inductive as G
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.RegAlloc.Liveness
import Backend.Operand
import Utils.Unique

data IGraph
  = IGraph {
    liveGraph :: G.Gr Reg (),
    moveGraph :: G.Gr Reg (),
    node2Reg :: Map G.Node Reg,
    reg2Node :: Map Reg G.Node
  }

mLiveGraph f = \st -> st { liveGraph = f (liveGraph st) }
mMoveGraph f = \st -> st { moveGraph = f (moveGraph st) }
mV2R f = \st -> st { node2Reg = f (node2Reg st) }
mR2V f = \st -> st { reg2Node = f (reg2Node st) }

buildIGraph :: (Instruction a, MonadUnique m) =>
               Fg.FlowGraph (Liveness a) -> m IGraph
buildIGraph fg = go
  where
    blockList = Map.elems (Fg.blockMap fg)
    flattenedInstrs = concatMap toList blockList

    emptyGraph = IGraph {
      liveGraph = G.empty,
      moveGraph = G.empty,
      node2Reg = Map.empty,
      reg2Node = Map.empty
    }

    go = flip execStateT emptyGraph $ do
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
          modify (mLiveGraph (G.insEdges [(nX, nY, ()), (nY, nX, ())]))
        addLiveRange ys
      _ -> return ()

    addNode r = do
      mbN <- gets (Map.lookup r . reg2Node)
      case mbN of
        Just oldNode -> do
          return oldNode
        Nothing -> do
          newNode <- mkUnique
          modify (mLiveGraph (G.insNode (newNode, r)))
          return newNode

    addMove i = do
      let [dest] = toList (defs i)
          [src] = toList (uses i)
        -- physReg -> physReg moves is added as well, although we cannot
        -- coalesce them. Doesn't matter, since we ignore them in the
        -- coalescing step.
      nDest <- addNode dest
      nSrc <- addNode src
      modify (mMoveGraph (G.insEdge (nSrc, nDest, ())))

minimumInterferenceNode :: IGraph -> G.Node
minimumInterferenceNode _ = 0

