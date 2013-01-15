module Backend.X64.OptZero.Frame (
  Frame(..),
  GcMap(..),
  FrameM,
  evalFrameM,

  mkFrameAddr, mkBaseIndexAddr, mkBaseAddr,
  argOps,
) where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Middleend.FlowGraph.Builder as Fg
import Backend.Operand
import Backend.RegAlloc.Liveness
import Backend.X64.Instr
import Utils.Unique

natSize = 8

data Frame
  = Frame {
    flowGraph :: Fg.FlowGraph (Liveness Instr),
    stackPtr :: Int, -- non-neg, to be sub'd in the prologue
    gcptrOffsets :: Set Int, -- by bytes
    regMap :: Map RegId Int,  -- RegId -> frame ptr offset
    gcMaps :: [GcMap]
  }

mStackPtr f = \st -> st { stackPtr = f (stackPtr st) }
modifyStackPtr = modify . mStackPtr
mGcptrOffsets f = \st -> st { gcptrOffsets = f (gcptrOffsets st) }
modifyGcptrOffsets = modify . mGcptrOffsets
mRegMap f = \st -> st { regMap = f (regMap st) }
modifyRegMap = modify . mRegMap
addGcMap m = \st -> st { gcMaps = m:gcMaps st }

data GcMap
  = GcMap {
    gmLabel :: Imm, -- the label for return addr
    gmGcptrOffsets :: [Int], -- relative to rbp
    gmGcptrRegs :: [Reg] -- callee-saved registers that contain gcptrs
  }

type FrameM = StateT Frame UniqueM

evalFrameM :: Fg.FlowGraph (Liveness Instr) -> FrameM a -> UniqueM a
evalFrameM g = (`evalStateT` emptyFrame)
  where
    emptyFrame = Frame {
      flowGraph = g,
      stackPtr = 0,
      gcptrOffsets = Set.empty,
      regMap = Map.empty,
      gcMaps = []
    }

-- Allocate (or retrive if allocated previously) a register on the stack
-- and return its address
stackAlloc :: Reg -> FrameM Operand
stackAlloc r = case r of
  RegP _ -> return (OpReg r)
  RegV (VirtualReg (rid, _, _, _)) -> do
    mb <- gets (Map.lookup rid . regMap)
    case mb of
      Just offset -> do
        return . OpAddr . mkFrameAddr $ offset
      Nothing -> do
        offset <- gets (negate . stackPtr)
        modifyStackPtr (+natSize)
        modifyRegMap (Map.insert rid offset)
        return . OpAddr . mkFrameAddr $ offset

markGcPtr :: Reg -> FrameM ()
markGcPtr (RegV (VirtualReg (rid, _, _, _))) = do
  Just offset <- gets (Map.lookup rid . regMap)
  modifyGcptrOffsets (Set.insert offset)

mkFrameAddr i = MkAddr {
  baseOfAddr = Just rbp,
  indexOfAddr = Nothing,
  dispOfAddr = Just (IntVal (fromIntegral i))
}

mkBaseIndexAddr base index = MkAddr {
  baseOfAddr = Just base,
  indexOfAddr = Just (index, 1),
  dispOfAddr = Nothing
}

mkBaseAddr base = MkAddr {
  baseOfAddr = Just base,
  indexOfAddr = Nothing,
  dispOfAddr = Nothing
}

argOps = map OpReg [rdi, rsi, rdx, rcx, r8, r9]

