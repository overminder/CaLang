module Middleend.FlowGraph.Trace (
  mkTrace
) where

-- Linearize a given flowgraph using flow information of basicblocks.
-- Pseudocode:
--
-- function mkTrace(graph):
-- groups = []
-- while there are more than one node:
--   for each node in the graph:
--     if node will fall through:
--       combine the node with its direct successor
--     if one of the node's pred will fallthrough to this one:
--       combine the pred with this node
--     insert combined nodes into groups
--     remove combined nodes from the graph
-- return groups

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Middleend.FlowGraph.Builder
import Backend.Operand

mkTrace :: Instruction a => FlowGraph a -> [BlockId]
mkTrace g = case rest2 of
  entryGroup:others -> entryGroup ++ concat (rest1 ++ others)
  _ -> error $ "mkTrace: cannot find entry: " ++ show (rest1, rest2)
  where
    groups = groupify g
    (rest1, rest2) = span isNotEntry groups
    isNotEntry bids = case bids of
      [] -> True
      x:xs -> x /= entryBlock g

type BlockGroup = [BlockId]

groupify :: Instruction a => FlowGraph a -> [BlockGroup]
groupify g = go g [] (Set.fromList (Map.keys (blockMap g)))
  where
    go g groups nodes =
      if Set.null nodes
        then groups
        else let node = Set.findMin nodes
                 newGroup = expandPreds g node ++ [node] ++ expandSuccs g node
                 rest = foldr Set.delete nodes newGroup
              in go g (newGroup:groups) rest

expandPreds g node = case directPred of
  [] -> []
  [b] -> let mbTarget = getFallThroughTarget (lastInstrOfBlock b)
             bid = blockId b
          in case mbTarget of
               Nothing -> -- is a call
                 expandPreds g bid ++ [bid]
               Just (BlockLabel node') ->
                 if node == node'
                   then expandPreds g bid ++ [bid]
                   else []
  where
    preds = map (blockMap g Map.!)
                (Set.toList (Map.findWithDefault Set.empty node (predMap g)))
    directPred = filter (isFallThroughInstr . lastInstrOfBlock) preds

expandSuccs g node = case directSucc of
  [] -> []
  [mbLabel] -> case mbLabel of
                 Just (BlockLabel bid) ->
                   bid : expandSuccs g bid
                 Nothing -> -- is a call
                   let succ = Set.findMin (succMap g Map.! node)
                    in succ : expandSuccs g succ
  where
    lastInstr = lastInstrOfBlock (blockMap g Map.! node)
    directSucc = if isFallThroughInstr lastInstr
                   then [getFallThroughTarget lastInstr]
                   else []

