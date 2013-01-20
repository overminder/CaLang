module Backend.RegAlloc.Coalescing (
  coalesce,
) where

import Prelude hiding (elem, notElem)
import Control.Monad.State
import Control.Monad.Identity
import Data.Function
import Data.Foldable
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

-- (Dest, Src)
type MovePair = (Reg, Reg)

pprMovePair (r1, r2) = parens (pprReg r1 <> comma <+> pprReg r2)

coalesce :: (MonadUnique m, Instruction a) =>
            Fg.FlowGraph (Liveness a) -> IGraph ->
            FuelT m (Fg.FlowGraph (Liveness a), IGraph)
coalesce fGraph iGraph = do
  remainingFuel <- get
  if remainingFuel == 0
    then return (fGraph, iGraph)
    else case findCoalescable iGraph of
      Nothing -> return (fGraph, iGraph)
      Just (dest, src) -> do
        --_ <- trace ("try coalescing " ++
        --            show (pprMovePair (dest, src)) ++
        --            " with fuel = " ++ show remainingFuel) $
        --       return ()
        let (fg', _) = runIdentity (Fg.iterAnalysis (Fg.Alter alterInstr)
                                                    fGraph)
            replaceReg r = if isPhysicalReg src
                             then if r == dest
                               then copyGcFlag r (copyOpWidth r src)
                               else r
                           else if isPhysicalReg dest
                             then if r == src
                               then copyGcFlag r (copyOpWidth r dest)
                               else r
                           else -- Both are virtual. Handle with caution.
                                -- That is, only coalesce if they share
                                -- the same GcFlag.
                                -- Another thing to notice is that in this
                                -- case we can kill either src or dest.
                                -- And we choose to kill dest.
                             if ((==) `on` gcFlagOfReg) dest src
                               then if r == dest
                                 then copyOpWidth r src
                                 else r
                               else r
            alterInstr i = case isSimpleMoveInstr i of
              True -> if Set.findMin (uses i) == src &&
                         Set.findMin (defs i) == dest
                        then if src == returnReg
                               then return ([mkNopInstr], True)
                               else return ([], True)
                        else return ([replaceRegInInstr replaceReg i], True)
              False -> return ([replaceRegInInstr replaceReg i], True)
            fg'' = iterDCE . iterLiveness . fmap instr $ fg'
        ig' <- lift $ buildIGraph fg''
        modify (flip (-) 1)
        coalesce fg'' ig'
        --return (fg'', ig')

findCoalescable :: IGraph -> Maybe MovePair
findCoalescable g = case coalescables of
  x:xs -> 
    Just x
  [] -> Nothing
  where
    moves :: [MovePair]
    moves = Map.foldrWithKey mkPair [] (moveEdges g)

    mkPair :: VertexId -> Set VertexId -> [MovePair] -> [MovePair]
    mkPair src dests out = map (flip (,) (v2r Map.! src))
                               (map (v2r Map.!) (toList dests)) ++ out

    interfEdges = edges g
    coalescables = filter (not . isForbidden) (filter (not . doesInterf) moves)

    v2r = vertexToReg g
    r2v = regToVertex g

    doesInterf :: MovePair -> Bool
    doesInterf (dest, src) =
      let srcV = r2v Map.! src
          destV = r2v Map.! dest
       in Set.member srcV (interfEdges Map.! destV)

    isForbidden (dest, src)
      | isPhysicalReg dest = dest `elem` forbiddenRegs
      | isPhysicalReg src = src `elem` forbiddenRegs
      | otherwise = False

-- We cannot coalesce those since they will be living across the whole
-- function but their liveness is only known after platform-dependent
-- code insertion so findCoalescable will report wrongly.
-- Therefore, we manually restrict coalescing of those registers.
forbiddenRegs = [stackPtrReg, framePtrReg]

