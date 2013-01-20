module Backend.X64.OptZero.Peephole (
  peepholeOpt
) where

import Control.Monad.Identity

import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Analysis as Fg
import Backend.Operand
import Backend.X64.Instr

peepholeOpt :: Fg.FlowGraph Instr -> Fg.FlowGraph Instr
peepholeOpt g =
  let (g', _) = runIdentity (Fg.iterAnalysis (Fg.Alter eliminateNop) g)
   in g'

eliminateNop :: Monad m => Instr -> m ([Instr], Bool)
eliminateNop i = return $ case i of
  NOP -> ([], True)
  _ -> ([i], False)

