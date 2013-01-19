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

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.RegAlloc.Interference
import Backend.Operand

type GraphStack = [VertexId]

-- We assume no spill/reload is need ATM.
allocPhysReg :: [Reg] -> IGraph -> Map VertexId Reg
allocPhysReg physRegs g = rebuild simplified g Map.empty
  where
    simplified = simplify g []

    simplify :: IGraph -> GraphStack -> GraphStack
    simplify g stack = case Set.null (vertices g) of
      True -> stack
      False -> let minV = vertexWithMinimumDegree g
                   g' = removeVertex minV g
                in simplify g' (minV : stack)

    rebuild :: GraphStack -> IGraph -> Map VertexId Reg -> Map VertexId Reg
    rebuild stack g assign = case stack of
      v:vs ->
        let origReg = (vertexToReg g) Map.! v
            assignedReg = findUsableReg neighbours assign (vertexToReg g)
            neighbours = Map.findWithDefault Set.empty v (edges g)
         in if isPhysicalReg origReg
              then rebuild vs g (Map.insert v origReg assign)
              else rebuild vs g (Map.insert v assignedReg assign)
      [] -> assign

    -- Exclude assigned regs and precolord regs from reg pool.
    findUsableReg :: Set VertexId -> Map VertexId Reg ->
                     Map VertexId Reg -> Reg
    findUsableReg neighbours assign v2r =
      let neighbourRegs = Set.foldr combine [] neighbours
          combine v rs = case Map.lookup v assign of
            Nothing -> case Map.lookup v v2r of
              -- In case there are precolored neighbours
              Nothing -> rs
              Just precolored -> precolored:rs
            Just r -> r:rs
       in case physRegs List.\\ neighbourRegs of
            x:_ -> x
            [] -> error $ "findUsableReg: out of regs!"

assignPhysReg :: Instruction a => Map VertexId Reg -> IGraph ->
               Fg.FlowGraph a -> Fg.FlowGraph a
assignPhysReg assign interfGraph flowGraph
  = fmap (replaceRegInInstr replacer) flowGraph
  where
    replacer r
      = if isVirtualReg r
          then case Map.lookup r (regToVertex interfGraph) of
                 Just v -> case Map.lookup v assign of
                   Just r' -> copyGcFlag r . copyOpWidth r $ r'
                   Nothing ->
                     error $ "assignPhysReg: Reg not allocated for " ++ show r
                 Nothing ->
                   error $ "assignPhysReg: No liveness info for reg " ++ show r
          else r


