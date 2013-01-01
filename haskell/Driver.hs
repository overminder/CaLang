module Driver (
  runAllPasses,
  outputPass,
  OutputOpt(..),
) where

import Control.Monad
import Text.Dot
import Text.PrettyPrint

import Frontend.AST
import Frontend.Parser
import Frontend.Rename
import qualified Frontend.Simplify as FrSim
import qualified Middleend.Tac.Munch as TacMun
import Middleend.FlowGraph.Builder
import qualified Middleend.FlowGraph.Simplify as GrSim
import Middleend.Tac.Instr
import Middleend.Tac.LocalOpt
import Backend.Operand
--import Backend.HOST_ARCH.Munch
--import Backend.HOST_ARCH.Instr
import Utils.Unique
import Utils.Class

data OutputOpt
  = OutputNothing
  | OutputRdrProg
  | OutputFrSim
  | OutputRawInstr
  | OutputRawDot
  | OutputLocOptDot

runAllPasses s = runUniqueM $ do
  let rdrProg = runParsePass s
  frRes <- runFrontendPass rdrProg
  grRes <- runGraphPass frRes
  return (frRes, grRes)

outputPass ((prog, _, fs), (iss, rgs, logs)) opts = do
  mapM_ show_opt opts
  where
    show_opt opt = case opt of
      OutputNothing -> return ()
      OutputRdrProg -> putStrLn . show . pprProgram $ prog
      OutputFrSim -> mapM_ (putStrLn . show . pprFunc) fs
      OutputRawInstr -> do
        forM_ (zip fs iss) $ \(f, is) -> do
          putStrLn $ "# Entry for <" ++ show (pprSignature f) ++ ">"
          mapM_ (putStrLn . show . ppr) is
      OutputRawDot -> output_graphs fs rgs
      OutputLocOptDot -> output_graphs fs logs

    output_graphs fs gs = do
        dots <- forM (zip fs gs) $ \(f, g) -> do
          return $ graphToDot (show (pprSignature f)) g
        let combined = sequence (map scope dots)
        putStrLn . showDot $ combined

runParsePass :: String -> Program String
runParsePass = readProgram

runFrontendPass :: Program String ->
                   UniqueM (Program String,
                            ([String], [Operand], [Data Operand]),
                            [Func Operand])
runFrontendPass rdrProg = do
  (rnDatas, rnFuncs, exports, clobRegs) <- runRenameM (rename rdrProg)
  (simDatas, simFuncs) <- FrSim.runSimplifyM (FrSim.simplify rnDatas rnFuncs)
  return (rdrProg, (exports, clobRegs, simDatas), simFuncs)

runGraphPass (_, _, simFuncs) = do
  insnss <- mapM (TacMun.runMunchM . TacMun.munch) simFuncs
  rawGraphs <- mapM (runGraphBuilderM . buildGraph) insnss
  let simGraphs = map GrSim.simplify rawGraphs
  locOptGrs <- mapM runOpt simGraphs
  return (insnss, simGraphs, locOptGrs)

pprSignature (Func name args _)
  = ppr name <> (parens (hcat (punctuate comma (map pprBinding args))))

