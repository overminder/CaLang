module Driver (
  driverMain
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import System.IO
import System.Environment
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
import Utils.OptParse

data Option
  = Option {
    optInput :: Handle,
    optOutput :: Handle,
    optOutputLevel :: OutputOpt
  }

data OutputOpt
  = OutputNothing
  | OutputRdrProg
  | OutputFrSim
  | OutputRawInstr
  | OutputRawDot
  | OutputLocOptDot

parseDriverOpt args = parseOpt options args emptyOption
  where
    emptyOption = Option stdin stdout OutputNothing
    setOutputLevel wat = \x -> return $ x { optOutputLevel = wat }
    options = [ BoolOption ["--rdrprog"] (setOutputLevel OutputRdrProg)
              , BoolOption ["--frsim"] (setOutputLevel OutputFrSim)
              , BoolOption ["--instr"] (setOutputLevel OutputRawInstr)
              , BoolOption ["--dot"] (setOutputLevel OutputRawDot)
              , BoolOption ["--locopt"] (setOutputLevel OutputLocOptDot)
              , NamedStringOption ["-o"] setOutput
              , StringOption setInput
              ]
    setInput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path ReadMode
        return $ opt { optInput = h }
    setOutput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path WriteMode
        return $ opt { optOutput = h }

runAllPasses s = runUniqueM $ do
  let rdrProg = runParsePass s
  frRes <- runFrontendPass rdrProg
  grRes <- runGraphPass frRes
  return (frRes, grRes)

driverMain = do
  args <- getArgs
  opt <- parseDriverOpt args
  flip runReaderT opt $ do
    h <- asks optInput
    src <- lift $ hGetContents h
    let result = runAllPasses src
    outputPass result

outputPass ((prog, _, fs), (iss, rgs, logs)) = do
  h <- asks optOutput
  level <- asks optOutputLevel
  case level of
    OutputNothing -> return ()
    OutputRdrProg -> lift . hPutStrLn h . show . pprProgram $ prog
    OutputFrSim -> mapM_ (lift . putStrLn . show . pprFunc) fs
    OutputRawInstr -> do
      forM_ (zip fs iss) $ \(f, is) -> do
        lift . hPutStrLn h $ "# Entry for <" ++ show (pprSignature f) ++ ">"
        mapM_ (lift . hPutStrLn h . show . ppr) is
    OutputRawDot -> output_graphs fs rgs
    OutputLocOptDot -> output_graphs fs logs

    where
    output_graphs fs gs = do
        h <- asks optOutput
        dots <- forM (zip fs gs) $ \(f, g) -> do
          return $ graphToDot (show (pprSignature f)) g
        let combined = sequence (map scope dots)
        lift . hPutStrLn h . showDot $ combined

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

