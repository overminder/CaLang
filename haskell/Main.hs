import Control.Monad

import Frontend.AST
import Frontend.Parser
import Frontend.Rename
import Frontend.Simplify

import Utils.Unique

compileProg rdrProg = do
  (rnData, rnFunc, exports, clobberedRegs) <- runRenameM (rename rdrProg)
  (simData, simFunc) <- runSimplifyM (simplify rnData rnFunc)
  return (simData, simFunc)

main = do
  rdrProg <- liftM readProgram getContents
  --putStrLn "RdrProg:"
  --putStrLn "********"
  --putStrLn . show . pprProgram $ rdrProg

  let (datas, funcs) = runUniqueM (compileProg rdrProg)

  putStrLn "SimFunc:" >> putStrLn "*******"
  mapM_ (putStrLn . show . pprFunc) funcs
  putStrLn "SimData:" >> putStrLn "*******"
  mapM_ (putStrLn . show . pprData) datas

