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
import qualified Frontend.Simplify as Sim
import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Simplify as Fg
import qualified Middleend.Tac.Munch as Tac
import qualified Middleend.Tac.Instr as Tac
import qualified Middleend.Tac.LocalOpt as Tac
import Backend.Operand
--import qualified Backend.HOST_ARCH.Munch as Arch
--import qualified Backend.HOST_ARCH.Instr as Arch
import qualified Backend.HOST_ARCH.OptZero.Munch as Arch_O
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
  | OutputOptZero

parseDriverOpt args = parseOpt options args emptyOption
  where
    emptyOption = Option stdin stdout OutputNothing
    setOutputLevel wat = \x -> return $ x { optOutputLevel = wat }
    options = [ BoolOption ["--rdrprog"] (setOutputLevel OutputRdrProg)
              , BoolOption ["--frsim"] (setOutputLevel OutputFrSim)
              , BoolOption ["--instr"] (setOutputLevel OutputRawInstr)
              , BoolOption ["--dot"] (setOutputLevel OutputRawDot)
              , BoolOption ["--locopt"] (setOutputLevel OutputLocOptDot)
              , BoolOption ["--O0"] (setOutputLevel OutputOptZero)
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

runAllPasses s = evalUniqueM $ do
  let rdrProg = runParsePass s
  frRes <- runFrontendPass rdrProg
  grRes <- runGraphPass frRes
  oOs <- runOptZeroPass frRes
  return (frRes, grRes, oOs)

driverMain = do
  args <- getArgs
  opt <- parseDriverOpt args
  flip runReaderT opt $ do
    h <- asks optInput
    src <- lift $ hGetContents h
    let result = runAllPasses src
    outputPass result

outputPass ((prog, _, fs), (iss, rgs, logs), optZeros) = do
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
    --OutputOptZero -> output_optZeros optZeros

    where
    output_graphs fs gs = do
        h <- asks optOutput
        dots <- forM (zip fs gs) $ \(f, g) -> do
          return $ Fg.graphToDot g
        let combined = sequence (map scope dots)
        lift . hPutStrLn h . showDot $ combined

    output_optZeros insnss = do
      h <- asks optOutput
      forM_ insnss $ \(name, insns) -> do
        lift . hPutStrLn h $ show (ppr name) ++ ":"
        lift . hPutStrLn h . show . vcat . map ppr $ insns

runParsePass :: String -> Program String
runParsePass = readProgram

runFrontendPass :: Program String ->
                   UniqueM (Program String,
                            ([String], [Operand], [Data Operand]),
                            [Func Operand])
runFrontendPass rdrProg = do
  (rnDatas, rnFuncs, exports, clobRegs) <- runRenameM (rename rdrProg)
  (simDatas, simFuncs) <- Sim.runSimplifyM (Sim.simplify rnDatas rnFuncs)
  return (rdrProg, (exports, clobRegs, simDatas), simFuncs)

runGraphPass (_, _, simFuncs) = do
  (insnss, rawGraphs) <- liftM unzip $ mapM mkInstrAndGraph simFuncs
  let simGraphs = map Fg.simplify rawGraphs
  locOptGrs <- mapM Tac.runOpt simGraphs
  return (insnss, simGraphs, locOptGrs)
  where
  mkInstrAndGraph (Func (OpImm (NamedLabel name)) args body) = do
    insns <- Tac.runMunchM . Tac.munch $ body
    let regArgs = map (unReg . snd) args
    graph <- Fg.runGraphBuilderM name regArgs . Fg.buildGraph $ insns
    return (insns, graph)
  unReg (OpReg r) = r

runOptZeroPass (_, _, simFuncs) = do
  undefined

pprSignature (Func name args _)
  = ppr name <> (parens (hcat (punctuate comma (map pprBinding args))))

