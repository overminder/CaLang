module Backend.RegAlloc.IterCoalesce (
  coalesce,
) where

import Prelude hiding (elem, notElem, foldr)
import Control.Arrow hiding ((<+>))
import Control.Monad.State
import Control.Monad.Identity
import Data.Function
import Data.Foldable
import qualified Data.Graph.Inductive as G
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Set as Set

import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Analysis as Fg
import Backend.RegAlloc.IGraph
import Backend.RegAlloc.Liveness
import Backend.Operand

import Utils.Unique
import Utils.Class

import Debug.Trace

type FuelT = StateT Int
type RegPair = (Reg, Reg)

pprRegPair (r1, r2) = parens (pprReg r1 <> comma <+> pprReg r2)

coalesce :: (MonadUnique m, Instruction a) =>
            Fg.FlowGraph (Liveness a) -> IGraph ->
            FuelT m (Fg.FlowGraph (Liveness a), IGraph)
coalesce fGraph iGraph = go fGraph iGraph []
  where
    go fGraph iGraph killWiths = do
      fuel <- get
      if fuel == 0
        then applyKill fGraph iGraph killWiths
        else case findCoalescable iGraph of
          Nothing -> applyKill fGraph iGraph killWiths
          Just (dest, src) -> do
            trace ("coalesce " ++ show (pprRegPair (dest, src)) ++
                   " at fuel = " ++ show fuel) $ return ()
            let (kill, with) = if isPhysicalReg dest then (src, dest)
                               else if isPhysicalReg src then (dest, src)
                               else (dest, src)
            modify (\x -> x - 1)
            let iGraph' = coalesceRegPair iGraph (kill, with)
            go fGraph iGraph' ((kill, with) : killWiths)

applyKill :: (MonadUnique m, Instruction a) =>
             Fg.FlowGraph (Liveness a) -> IGraph -> [RegPair] ->
             FuelT m (Fg.FlowGraph (Liveness a), IGraph)
applyKill fGraph iGraph killWiths = case killWiths of
  [] -> return (fGraph, iGraph)
  _ -> do
    let fGraph' = fmap replace fGraph
        fg'' = iterDCE . iterLiveness . fmap instr $ fGraph'
    ig' <- buildIGraph fg''
    coalesce fg'' ig'
  where
    repMap = foldr mkRepMap Map.empty killWiths
    mkRepMap (kill, with) mp = Map.map (\r -> if r == kill then with else r)
                                       (Map.insert kill with mp)
    replace = replaceRegInInstr (\r -> Map.findWithDefault r r repMap)

coalesceRegPair :: IGraph -> RegPair -> IGraph
coalesceRegPair g (kill, with) = g { liveGraph = lg'
                                   , moveGraph = mg'
                                   }
  where
    lg' = redirectOn (liveGraph g)
    mg' = redirectOn (moveGraph g)

    redirectOn g
      = let g' = foldr redirectPred g (G.pre g vKill)
            g'' = foldr redirectSucc g' (G.suc g vKill)
         in G.delNode vKill g''

    redirectPred vPred
      = insNoDup vPred vWith . G.delEdge (vPred, vKill)
    redirectSucc vSucc
      = insNoDup vWith vSucc . G.delEdge (vKill, vSucc)

    insNoDup v1 v2 g = case v2 `elem` (G.suc g v1) of
      True -> g
      False -> if v1 /= v2 then G.insEdge (v1, v2, ()) g else g

    vKill = reg2Vertex g Map.! kill
    vWith = reg2Vertex g Map.! with

findCoalescable :: IGraph -> Maybe RegPair
findCoalescable g = case coalescables of
  x:xs -> 
    Just x
  [] -> Nothing
  where
    moves :: [RegPair]
    moves = map mkPair (G.edges (moveGraph g))

    mkPair :: (Vertex, Vertex) -> RegPair
    mkPair (src, dest) = (v2r Map.! src, v2r Map.! dest)

    coalescables = filters [ not . isForbidden
                           , not . doesInterf
                           , not . allPhys
                           ] moves

    v2r = vertex2Reg g
    r2v = reg2Vertex g

    doesInterf :: RegPair -> Bool
    doesInterf (dest, src) =
      let srcV = r2v Map.! src
          destV = r2v Map.! dest
       in srcV `elem` (G.suc (liveGraph g) destV)

    isForbidden (dest, src)
      | isPhysicalReg dest = dest `elem` forbiddenRegs
      | isPhysicalReg src = src `elem` forbiddenRegs
      | otherwise = False

    allPhys (dest, src) = ((&&) `on` isPhysicalReg) dest src

-- We cannot coalesce those since they will be living across the whole
-- function but their liveness is only known after platform-dependent
-- code insertion so findCoalescable will report wrongly.
-- Therefore, we manually restrict coalescing of those registers.
forbiddenRegs = [stackPtrReg, framePtrReg]

-- Left to right composition
filters :: [a -> Bool] -> [a] -> [a]
filters fs = filter f
  where
    f = foldr (\f g x -> f x && g x) (const True) fs

