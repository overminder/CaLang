import Control.Monad

import Frontend.AST
import Frontend.Parser
import Frontend.Rename
import Frontend.Simplify

import Backend.HOST_ARCH.Munch
import Backend.HOST_ARCH.Instr

import Utils.Unique

compileProg rdrProg = do
  (rnData, rnFunc, exports, clobberedRegs) <- runRenameM (rename rdrProg)
  (simData, simFunc) <- runSimplifyM (simplify rnData rnFunc)
  insnss <- forM simFunc $ \f@(Func name _ _) -> do
              insn <- runMunchM (munch f)
              return (show name, insn)
  return (simData, simFunc, insnss)

main = do
  rdrProg <- liftM readProgram getContents
  --putStrLn "RdrProg:"
  --putStrLn "********"
  --putStrLn . show . pprProgram $ rdrProg

  let (datas, funcs, insnss) = runUniqueM (compileProg rdrProg)

  putStrLn "SimFunc:" >> putStrLn "*******"
  mapM_ (putStrLn . show . pprFunc) funcs
  putStrLn "SimData:" >> putStrLn "*******"
  mapM_ (putStrLn . show . pprData) datas
  forM insnss $ \(comment, insns) -> do
    putStrLn $ "# " ++ comment
    mapM_ (putStrLn . show . pprInstr) insns
