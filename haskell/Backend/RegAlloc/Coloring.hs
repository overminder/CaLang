module Backend.RegAlloc.Coloring (
  color,
  materialize,
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

type GraphStack = [(VertexId, Set VertexId)]

-- We assume no spill/reload is need ATM.
color :: [Reg] -> Graph -> Map VertexId Reg
color physRegs g = rebuild simplified g Map.empty
  where
    simplified = simplify g []

    simplify :: Graph -> GraphStack -> GraphStack
    simplify g stack = case Set.null (vertices g) of
      True -> stack
      False -> let minV = vertexWithMinimumDegree g
                   neighbours = (edges g) Map.! minV
                   g' = removeVertex minV g
                in simplify g' ((minV, neighbours) : stack)

    rebuild :: GraphStack -> Graph -> Map VertexId Reg -> Map VertexId Reg
    rebuild stack g assign = case stack of
      (v, neighbours):xs ->
        let origReg = (vertexToReg g) Map.! v
            assignedReg = findUsableReg neighbours assign
         in if isPhysicalReg origReg
              then rebuild xs g (Map.insert v origReg assign)
              else rebuild xs g (Map.insert v assignedReg assign)
      [] -> assign

    findUsableReg :: Set VertexId -> Map VertexId Reg -> Reg
    findUsableReg neighbours assign =
      let neighbourRegs = Set.foldr combine [] neighbours
          combine = (:) . (Map.!) assign
       in case physRegs List.\\ neighbourRegs of
            x:_ -> x
            [] -> error $ "findUsableReg: out of regs!"

materialize :: Instruction a => Map VertexId Reg -> Graph ->
               Fg.FlowGraph a -> Fg.FlowGraph a
materialize assign interfGraph flowGraph
  = fmap (replaceRegInInstr replacer) flowGraph
  where
    replacer r
      = if isVirtualReg r
          then case Map.lookup r (regToVertex interfGraph) of
                 Just v -> case Map.lookup v assign of
                   Just r' -> copyGcFlag r . copyOpWidth r $ r'
                   Nothing ->
                     error $ "materialize: Reg not allocated for " ++ show r
                 Nothing ->
                   error $ "materialize: No liveness info for reg " ++ show r
          else r


