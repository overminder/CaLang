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
import Backend.Class

simplify :: forall a. Instruction a => FlowGraph a -> FlowGraph a
simplify g = run_pipeline passes
  where
    run_pipeline = foldl (flip ($)) g
    passes = [renameLocalJump,
              removeUnreachableBlock]

-- Set all local jump instr to reference their dest by block ids
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

-- If a block other than the entry has no pred block, then we can remove it
removeUnreachableBlock initG = remover initG
  where
    entry_id = entryBlock initG
    is_entry = (==entry_id) . blockId
    remover g = let blocks = blockMap g
                    preds = predMap g
                    has_pred b = Map.member (blockId b) preds
                    removed = Map.foldr select_remove [] blocks
                    select_remove b xs = if (has_pred b || is_entry b)
                      then xs
                      else blockId b:xs
                    preds' = foldr Map.delete preds removed
                    blocks' = foldr Map.delete blocks removed
                 in if null removed
                      then g
                      else remover g {
                        blockMap = blocks',
                        predMap = preds'
                      }


