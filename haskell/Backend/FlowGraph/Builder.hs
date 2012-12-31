{-# LANGUAGE DeriveFunctor, RankNTypes #-}
module Backend.FlowGraph.Builder (
  runGraphBuilderM,
  buildGraph,
  graphToDot,
  FlowGraph(..),
  BasicBlock(..),
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Dot

import Backend.HOST_ARCH.Instr
import Backend.Operand
import Backend.Class
import qualified Utils.Unique as Unique

type BlockId = Unique.Unique

data FlowGraph a
  = MkGraph {
    entryBlock :: BlockId,
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
  deriving (Functor)

data GraphBuilder a
  = MkBuilder {
    gbGraph :: FlowGraph a,
    gbCurrBlock :: BasicBlock a,
    gbLabelMap :: Map Imm BlockId, -- modified when new block is created
    gbLazyLinks :: [(BlockId, Imm)] -- resolved in the last
  }
  deriving (Functor)

type CompilerM = Unique.UniqueM
type GraphBuilderM = StateT (GraphBuilder Instr) CompilerM

mkUnique = lift Unique.mkUnique

runGraphBuilderM :: GraphBuilderM a -> CompilerM (FlowGraph Instr)
runGraphBuilderM m = do
  bdr <- execStateT m empty_builder
  return (gbGraph bdr) { labelMap = gbLabelMap bdr }
  where
    empty_builder = MkBuilder empty_graph empty_block Map.empty []
    empty_graph = MkGraph (-1) Map.empty Map.empty Map.empty (error "lblMap")

empty_block = MkBlock (-1) [] Nothing []

buildGraph :: [Instr] -> GraphBuilderM ()
buildGraph is = do
  entry <- newBlock
  modify $ \st -> st {
    gbGraph = (gbGraph st) { entryBlock = entry }
  }
  mapM_ addInstr is
  resolveLazyLinks

pass :: forall m. Monad m => m ()
pass = return ()

addInstr :: Instr -> GraphBuilderM ()
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

resolveLazyLinks :: GraphBuilderM ()
resolveLazyLinks = do
  links <- liftM gbLazyLinks get
  lblMap <- liftM gbLabelMap get
  forM_ links $ \(fromId, lbl) -> do
    case Map.lookup lbl lblMap of
      Just toId -> linkBlock fromId toId
      Nothing -> error $ "X64.resolveLazyLinks: unknown label: " ++ show lbl

finishCurrBlock :: GraphBuilderM ()
finishCurrBlock = do
  currBlock <- liftM gbCurrBlock get
  insertBlock currBlock
  modify $ \st -> st {
    gbCurrBlock = empty_block
  }

insertBlock :: BasicBlock Instr -> GraphBuilderM ()
insertBlock bb = do
  blkMap <- liftM (blockMap . gbGraph) get
  modify $ \st -> st {
    gbGraph = (gbGraph st) {
      blockMap = Map.insert (blockId bb) bb blkMap
    }
  }

newBlock :: GraphBuilderM BlockId
newBlock = do
  i <- mkUnique
  modify $ \st -> st {
    gbCurrBlock = empty_block { blockId = i }
  }
  return i

addLazyLink :: BlockId -> Imm -> GraphBuilderM ()
addLazyLink bid lbl = modify $ \st -> st {
  gbLazyLinks = (bid, lbl):gbLazyLinks st
}

linkBlock :: BlockId -> BlockId -> GraphBuilderM ()
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

addLabelMapping :: Imm -> BlockId -> GraphBuilderM ()
addLabelMapping i b = do
  modify $ \st -> st {
    gbLabelMap = Map.insert i b (gbLabelMap st)
  }

-- Dot file support
graphToDot :: String -> FlowGraph Instr -> Dot ()
graphToDot name (MkGraph eb bm pm sm _) = do
  nodes <- liftM Map.fromList $ forM (Map.toList bm) $ \(bid, bb) -> do
    -- for each block, create a node
    nid <- mk_block_node bb
    return (bid, nid)
  forM_ (Map.toList nodes) $ \(bid, nid) -> do
    -- for each block, link it to its successors
    case Map.lookup bid sm of
      Just succBlocks -> forM_ (Set.toList succBlocks) $ \sbid -> do
        case Map.lookup sbid nodes of
          Just snid -> nid .->. snid
          Nothing -> do
            error "graphToDot: no corresponding block"
      Nothing ->
        -- No successor
        pass
  entry <- mk_node $ "Entry for <" ++ name ++ ">"
  entry .->. (nodes Map.! eb)
  where
    mk_block_node (MkBlock bid is (Just ctrl) _)
      = mk_node (unlines (["<Block " ++ show bid ++ ">"] ++
                          map (show . pprInstr) is ++
                          ["# Control Instr:", show (pprInstr ctrl)]))
    mk_node label = node [("label", label),
                          ("shape", "box"),
                          ("fontsize", "8.00")]


