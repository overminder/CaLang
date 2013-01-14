module Backend.X64.OptZero.Frame (
  Frame(..),
  GcMap(..),
  FrameM,

  mkFrameAddr, mkBaseIndexAddr, mkBaseAddr,
) where

import Control.Monad.State

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Backend.Operand
import Backend.X64.Operand hiding (Reg)
import Backend.X64.Instr
import Utils.Unique

natSize = 8

data Frame
  = Frame {
    stackPtr :: Int, -- positive, to be sub'd in the prologue
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
    gmLabel :: Imm,
    gmGcptrOffsets :: [Int]
  }

type FrameM = StateT Frame UniqueM

-- Allocate (or retrive if allocated previously) a register on the stack
-- and return its address
stackAlloc :: Reg -> FrameM Operand
stackAlloc r = case r of
  PReg _ -> return (OpReg r)
  VReg rid _ _ _ -> do
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
markGcPtr (VReg rid _ _ _) = do
  Just offset <- gets (Map.lookup rid . regMap)
  modifyGcptrOffsets (Set.insert offset)

mkFrameAddr i = MkAddr {
  baseOfAddr = Just (PReg rbp),
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

