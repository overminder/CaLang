import Control.Monad

import Frontend.AST
import Frontend.Parser
import Frontend.Rename

import Utils.Unique

main = do
  rdrProg <- liftM readProgram getContents
  --putStrLn "RdrProg:"
  --putStrLn "********"
  --putStrLn . show . pprProgram $ rdrProg

  let (rnProg, exports, clobberedRegs) =
        runUniqueM . runRenameM . rename $ rdrProg
  putStrLn "RnProg:"
  putStrLn "*******"
  putStrLn . show . pprProgram $ rnProg

