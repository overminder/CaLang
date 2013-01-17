{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Middleend.FlowGraph.Builder (
  runGraphBuilderM,
  buildGraph,
  graphToDot,
  FlowGraph(..),
  BasicBlock(..), BlockId,

  hasNoSucc, hasNoPred,
  getBlock, putBlock, getSuccBlockIds, getPredBlockIds,
  firstInstrOfBlock, lastInstrOfBlock,
) where

import Debug.Trace

import Prelude hiding (mapM_)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Control.Monad.State hiding (forM, forM_, mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Dot
import Text.PrettyPrint

import Frontend.AST
import Backend.Operand
import Utils.Unique
import Utils.Class

type BlockId = Unique

data FlowGraph a
  = MkGraph {
    funcName :: String,
    funcArgs :: [Reg],
    funcConv :: Bool,
    entryBlock :: BlockId,
    blockTrace :: [BlockId],
    blockMap :: Map BlockId (BasicBlock a),
    predMap :: Map BlockId (Set BlockId),
    succMap :: Map BlockId (Set BlockId),
    labelMap :: Map Imm BlockId -- undefined while building
  }
  deriving (Functor)

data BasicBlock a
  = MkBlock {
    blockId :: BlockId,
    instrList :: [a],
    controlInstr :: Maybe a,
    blockLabels :: [Imm]
  }
  deriving (Functor, Foldable, Traversable)

data GraphBuilder a
  = MkBuilder {
    gbGraph :: FlowGraph a,
    gbCurrBlock :: BasicBlock a,
    gbLabelMap :: Map Imm BlockId, -- modified when new block is created
    gbLazyLinks :: [(BlockId, Imm)] -- resolved in the last
  }
  deriving (Functor)

type CompilerM = UniqueM
type GraphBuilderM instr = StateT (GraphBuilder instr) CompilerM

runGraphBuilderM :: Instruction instr => String -> [Reg] -> Bool ->
                    GraphBuilderM instr a -> CompilerM (FlowGraph instr)
runGraphBuilderM name args conv m = do
  bdr <- execStateT m empty_builder
  let g = (gbGraph bdr) { labelMap = gbLabelMap bdr }
  return g
  where
    empty_builder = MkBuilder empty_graph empty_block Map.empty []
    empty_graph = MkGraph {
      funcName = name,
      funcArgs = args,
      funcConv = conv,
      blockTrace = [], -- no trace yet
      entryBlock = (-1),
      blockMap = Map.empty,
      predMap = Map.empty,
      succMap = Map.empty,
      labelMap = error "lblMap"
    }

empty_block = MkBlock (-1) [] Nothing []

buildGraph :: Instruction a => [a] -> GraphBuilderM a ()
buildGraph is = do
  entry <- newBlock
  modify $ \st -> st {
    gbGraph = (gbGraph st) { entryBlock = entry }
  }
  mapM_ addInstr is
  resolveLazyLinks

addInstr :: Instruction a => a -> GraphBuilderM a ()
addInstr i
  | isBranchInstr i = do
      currBlock <- liftM gbCurrBlock get
      modify $ \st -> st {
        gbCurrBlock = (gbCurrBlock st) {
          controlInstr = Just i
        }
      }
      let oldBlockId = blockId currBlock
      finishCurrBlock
      newId <- newBlock
      mapM_ (addLazyLink oldBlockId) (localBranchTargets i)
      -- Since localBranchTarget does not include the next instr...
      if isFallThroughInstr i
        then linkBlock oldBlockId newId
        else pass

  | isLabelInstr i = do
      let label = getLabelOfInstr i
      currBlock <- liftM gbCurrBlock get
      if null (instrList currBlock)
        then pass
        else do
          let oldBlockId = blockId currBlock
          finishCurrBlock
          newId <- newBlock
          linkBlock oldBlockId newId
          -- Explicitly add jump instr if there is no control instr
          -- in the previous block.
          case controlInstr currBlock of
            Just _ -> pass
            Nothing -> do
              let instr = mkJumpInstr (BlockLabel newId)
              insertBlock (currBlock { controlInstr = Just instr })
      currBlockId <- liftM (blockId . gbCurrBlock) get
      addLabelMapping label currBlockId

  | otherwise = modify $ \st -> st {
      gbCurrBlock = (gbCurrBlock st) {
        instrList = instrList (gbCurrBlock st) ++ [i]
      }
    }

resolveLazyLinks :: GraphBuilderM a ()
resolveLazyLinks = do
  links <- liftM gbLazyLinks get
  lblMap <- liftM gbLabelMap get
  forM_ links $ \(fromId, lbl) -> do
    case Map.lookup lbl lblMap of
      Just toId -> linkBlock fromId toId
      Nothing -> error $ "X64.resolveLazyLinks: unknown label: " ++ show lbl

finishCurrBlock :: GraphBuilderM a ()
finishCurrBlock = do
  currBlock <- liftM gbCurrBlock get
  insertBlock currBlock
  modify $ \st -> st {
    gbCurrBlock = empty_block
  }

insertBlock :: BasicBlock a -> GraphBuilderM a ()
insertBlock bb = do
  blkMap <- liftM (blockMap . gbGraph) get
  modify $ \st -> st {
    gbGraph = (gbGraph st) {
      blockMap = Map.insert (blockId bb) bb blkMap
    }
  }

newBlock :: GraphBuilderM a BlockId
newBlock = do
  i <- mkUnique
  modify $ \st -> st {
    gbCurrBlock = empty_block { blockId = i }
  }
  return i

addLazyLink :: BlockId -> Imm -> GraphBuilderM a ()
addLazyLink bid lbl = modify $ \st -> st {
  gbLazyLinks = (bid, lbl):gbLazyLinks st
}

linkBlock :: BlockId -> BlockId -> GraphBuilderM a ()
linkBlock bFrom bTo = do
  g <- liftM gbGraph get
  modify $ \st -> st {
    gbGraph = g {
      predMap = insert_set bTo (Set.singleton bFrom) (predMap g),
      succMap = insert_set bFrom (Set.singleton bTo) (succMap g)
    }
  }
  where
    insert_set = Map.insertWithKey (const Set.union)

addLabelMapping :: Imm -> BlockId -> GraphBuilderM a ()
addLabelMapping i b = do
  modify $ \st -> st {
    gbLabelMap = Map.insert i b (gbLabelMap st)
  }

-- Accessors

hasNoSucc :: FlowGraph a -> BlockId -> Bool
hasNoSucc g bid = Set.null (getSuccBlockIds bid g)

hasNoPred :: FlowGraph a -> BlockId -> Bool
hasNoPred g bid = Set.null (getPredBlockIds bid g)

getBlock :: BlockId -> FlowGraph a -> BasicBlock a
getBlock bid g = (blockMap g) Map.! bid

putBlock :: BasicBlock a -> FlowGraph a -> FlowGraph a
putBlock b g = g {
  blockMap = Map.insert (blockId b) b (blockMap g)
}

getSuccBlockIds :: BlockId -> FlowGraph a -> Set BlockId
getSuccBlockIds bid g = Map.findWithDefault Set.empty bid (succMap g)

getPredBlockIds :: BlockId -> FlowGraph a -> Set BlockId
getPredBlockIds bid g = Map.findWithDefault Set.empty bid (predMap g)

firstInstrOfBlock :: BasicBlock a -> a
firstInstrOfBlock b = case instrList b ++ toList (controlInstr b) of
  x:xs -> x

lastInstrOfBlock :: BasicBlock a -> a
lastInstrOfBlock b = case instrList b ++ toList (controlInstr b) of
  xs -> last xs

-- Dot file support
graphToDot :: Ppr instr => FlowGraph instr -> Dot ()
graphToDot (MkGraph name args _ eb _ bm pm sm _) = do
  nodes <- liftM Map.fromList $ forM (Map.toList bm) $ \(bid, bb) -> do
    -- for each block, create a node
    nid <- mk_block_node bb
    return (bid, nid)
  forM_ (Map.toList nodes) $ \(bid, nid) -> do
    -- for each block, link it to its predecessors
    case Map.lookup bid pm of
      Just predBlocks -> forM_ (Set.toList predBlocks) $ \pbid -> do
        case Map.lookup pbid nodes of
          Just pnid -> pnid .->. nid
          Nothing -> do
            error $ "graphToDot: no corresponding pred block for " ++ show pbid
      Nothing ->
        -- No predecessor, assert is entry
        if bid /= eb
          then error $ "graphToDot: dangling block: " ++ show bid
          else pass
  entry <- mk_node $ "Entry for <" ++ show (pprSignature name args) ++ ">"
  entry .->. (nodes Map.! eb)
  where
    mk_block_node (MkBlock bid is (Just ctrl) _)
      = mk_node (unlines (["<Block " ++ show bid ++ ">"] ++
                          map (show . ppr) is ++
                          ["# Control Instr:", show (ppr ctrl)]))
    mk_node label = node [("label", label),
                          ("shape", "box"),
                          ("fontsize", "8.00")]


pprSignature :: String -> [Reg] -> Doc
pprSignature name args
  = text name <> parens (hcat (punctuate comma (map pprReg args)))

