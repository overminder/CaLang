{-# LANGUAGE RankNTypes #-}
module Middleend.FlowGraph.Simplify (
  simplify,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Dot

import Middleend.FlowGraph.Builder
import Backend.Operand

simplify :: forall a. Instruction a => FlowGraph a -> FlowGraph a
simplify g = run_pipeline passes
  where
    run_pipeline = foldl (flip ($)) g
    passes = [renameLocalJump,
              removeUnreachableBlock]

-- Set all local jump instr to reference their dest by block ids
-- XXX: other instrs that refer to local labels are not renamed.
renameLocalJump g = g'
  where
    labels = labelMap g
    g' = g {
      blockMap = Map.map change_block_instr (blockMap g)
    }
    change_block_instr bb = bb {
      controlInstr = fmap (renameBranchInstrLabel change_label)
                          (controlInstr bb)
    }
    change_label lbl = case Map.lookup lbl labels of
      Just bid -> BlockLabel bid
      _ -> lbl

-- If a block other than the entry has no pred block, then we can remove it.
-- Algorithm in imperative pseudo-code:
-- def remove_unreachable_block(graph):
--     removed_blocks = []
--     for block in graph:
--         preds = graph.find_pred(block)
--         if not preds and graph.entry != block:
--             removed_blocks.append((block, graph.find_succ(block)))
--             graph.blocks.remove(block)
--             graph.preds.remove(block)
--             graph.succs.remove(block)
--
--     if not removed_blocks:
--         return
--     else:
--         for block, succs in removed_blocks:
--             for succ in succs:
--                 its_preds = graph.find_pred(succ)
--                 its_preds.remove(block)
--         return remove_unreachable_block(graph)
removeUnreachableBlock :: forall a. FlowGraph a -> FlowGraph a
removeUnreachableBlock graph = case removed_blocks of
  [] -> graph
  _ -> removeUnreachableBlock graph'
  where
    blocks = blockMap graph
    entry_id = entryBlock graph
    is_not_entry = (/=entry_id)
    block_ids = Map.keys blocks
    succs = succMap graph
    preds = predMap graph
    has_no_pred :: BlockId -> Bool
    has_no_pred b = case Map.lookup b preds of
      Nothing -> True
      Just some -> Set.null some
    removed_blocks = foldr find_block_to_remove [] block_ids
    find_block_to_remove block_id xs
      = if has_no_pred block_id && is_not_entry block_id
          then (block_id, Map.findWithDefault Set.empty block_id succs):xs
          else xs
    blocks' = foldr Map.delete blocks (map fst removed_blocks)
    succs' = foldr Map.delete succs (map fst removed_blocks)
    preds' = foldr Map.delete preds (map fst removed_blocks)
    preds'' = foldr remove_from_preds preds' removed_blocks
    remove_from_preds (bid, bsuccs) all_preds
      = Set.foldr remove_from_one all_preds bsuccs
      where
        remove_from_one sbid pmap
          = let its_preds = Set.delete bid (pmap Map.! sbid)
             in case Set.null its_preds of
                  True -> Map.delete sbid pmap
                  False -> Map.insert sbid its_preds pmap
    graph' = graph {
      blockMap = blocks',
      succMap = succs',
      predMap = preds''
    }

