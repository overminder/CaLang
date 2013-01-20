module Backend.RegAlloc.Coloring (
  allocPhysReg,
  assignPhysReg,
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Graph.Inductive as G

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.RegAlloc.IGraph
import Backend.Operand

type GraphStack = [Vertex]

-- We assume no spill/reload is need ATM.
allocPhysReg :: [Reg] -> IGraph -> Map Vertex Reg
allocPhysReg physRegs g = rebuild simplified g Map.empty
  where
    simplified = simplify g []

    simplify :: IGraph -> GraphStack -> GraphStack
    simplify g stack = case G.isEmpty (liveGraph g) of
      True -> stack
      False -> let minN = minimumInterferenceNode g
                   g' = removeLiveRange minN g
                in simplify g' (minN : stack)

    rebuild :: GraphStack -> IGraph -> Map Vertex Reg -> Map Vertex Reg
    rebuild stack g assign = case stack of
      n:ns ->
        let origReg = Map.findWithDefault (error "not found") n (vertex2Reg g)
            assignedReg = findUsableReg neighbours assign (vertex2Reg g)
            neighbours = G.suc (liveGraph g) n
         in if isPhysicalReg origReg
              then rebuild ns g (Map.insert n origReg assign)
              else rebuild ns g (Map.insert n assignedReg assign)
      [] -> assign

    -- Exclude assigned regs and precolord regs from reg pool.
    findUsableReg :: [Vertex] -> Map Vertex Reg ->
                     Map Vertex Reg -> Reg
    findUsableReg neighbours assign v2r =
      let neighbourRegs = foldr combine [] neighbours
          combine v rs = case Map.lookup v assign of
            Nothing -> case Map.lookup v v2r of
              -- In case there are precolored neighbours
              Nothing -> rs
              Just precolored -> precolored:rs
            Just r -> r:rs
       in case physRegs List.\\ neighbourRegs of
            x:_ -> x
            [] -> error $ "findUsableReg: out of regs!"

assignPhysReg :: Instruction a => Map Vertex Reg -> IGraph ->
               Fg.FlowGraph a -> Fg.FlowGraph a
assignPhysReg assign interfGraph flowGraph
  = fmap (replaceRegInInstr replacer) flowGraph
  where
    replacer r
      = if isVirtualReg r
          then case Map.lookup r (reg2Vertex interfGraph) of
                 Just v -> case Map.lookup v assign of
                   Just r' -> copyGcFlag r . copyOpWidth r $ r'
                   Nothing ->
                     error $ "assignPhysReg: Reg not allocated for " ++ show r
                 Nothing ->
                   error $ "assignPhysReg: No liveness info for reg " ++ show r
          else r


