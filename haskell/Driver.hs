module Driver (
  driverMain
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
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
import qualified Backend.HOST_ARCH.OptZero.Munch as Arch
import qualified Backend.HOST_ARCH.OptZero.Frame as Arch
import qualified Backend.RegAlloc.Liveness as Ral
import qualified Backend.RegAlloc.Interference as Ral
import qualified Backend.RegAlloc.Coloring as Ral
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

  | OutputOptZeroDot
  | OutputOptZeroLiveness
  | OutputOptZeroInterf
  | OutputOptZeroColor
  | OutputOptZeroRegAlloc

parseDriverOpt asimGs = parseOpt options asimGs emptyOption
  where
    emptyOption = Option stdin stdout OutputNothing
    setOutputLevel wat = \x -> return $ x { optOutputLevel = wat }
    options = [ BoolOption ["--rdrprog"] (setOutputLevel OutputRdrProg)
              , BoolOption ["--frsim"] (setOutputLevel OutputFrSim)
              , BoolOption ["--instr"] (setOutputLevel OutputRawInstr)
              , BoolOption ["--dot"] (setOutputLevel OutputRawDot)
              , BoolOption ["--locopt"] (setOutputLevel OutputLocOptDot)
              , BoolOption ["--O0-dot"] (setOutputLevel OutputOptZeroDot)
              , BoolOption ["--O0-lv"] (setOutputLevel OutputOptZeroLiveness)
              , BoolOption ["--O0-interf"] (setOutputLevel OutputOptZeroInterf)
              , BoolOption ["--O0-color"] (setOutputLevel OutputOptZeroColor)
              , BoolOption ["--O0-ral"] (setOutputLevel OutputOptZeroRegAlloc)
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
  oOs <- runOptZeroPass grRes
  return (frRes, grRes, oOs)

driverMain :: IO ()
driverMain = do
  args <- getArgs
  opt <- parseDriverOpt args
  flip runReaderT opt $ do
    h <- asks optInput
    src <- lift $ hGetContents h
    let result = runAllPasses src
    outputPass result

outputPass ( (prog, _, fs)
           , (iss, simGs, locOptGs)
           , (natGs, lvNatGs, interfGs, colors, ralGs)) = do
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
    OutputRawDot -> output_flowGraphs simGs
    OutputLocOptDot -> output_flowGraphs locOptGs
    OutputOptZeroDot -> output_flowGraphs natGs
    OutputOptZeroLiveness -> output_flowGraphs lvNatGs
    OutputOptZeroInterf -> output_interfGraphs interfGs
    OutputOptZeroColor -> do
      let dots = map mkDot (zip interfGs colors)
          mkDot (graph, color) = 
            let showVertex v = show $ pprReg (color Map.! v)
             in Ral.graphToDot graph showVertex
      lift . hPutStrLn h . showDot . combine_dots $ dots
    OutputOptZeroRegAlloc -> output_flowGraphs ralGs

    where
    combine_dots :: [Dot ()] -> Dot ()
    combine_dots = sequence_ . map scope

    output_flowGraphs :: Ppr a => [Fg.FlowGraph a] -> ReaderT Option IO ()
    output_flowGraphs gs = do
      h <- asks optOutput
      let dots = map Fg.graphToDot gs
      lift . hPutStrLn h . showDot . combine_dots $ dots

    output_interfGraphs :: [Ral.Graph] -> ReaderT Option IO ()
    output_interfGraphs gs = do
      h <- asks optOutput
      let dots = map Ral.rawGraphToDot gs
      lift . hPutStrLn h . showDot . combine_dots $ dots

runParsePass :: String -> Program String
runParsePass = readProgram

runFrontendPass :: Program String ->
                   UniqueM (Program String,
                            ([String], [Reg], [Data Operand]),
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
  mkInstrAndGraph (Func (OpImm (NamedLabel name)) asimGs body isC) = do
    insns <- Tac.runMunchM . Tac.munch $ body
    let regAsimGs = map (unReg . snd) asimGs
    graph <- Fg.runGraphBuilderM name regAsimGs isC . Fg.buildGraph $ insns
    return (insns, graph)
  unReg (OpReg r) = r

runOptZeroPass (_, simGs, _) = do
  let natGs = map (Arch.evalMunchM . Arch.munchGraph) simGs
      lvNatGs = map Ral.iterLiveness natGs
  interfGs <- mapM Ral.buildGraph lvNatGs
  let colors = map (Ral.color generalRegs) interfGs
      ralGs = zipWith3 Ral.materialize colors interfGs lvNatGs
  return (natGs, lvNatGs, interfGs, colors, ralGs)

pprSignature (Func name asimGs _ _)
  = ppr name <> (parens (hcat (punctuate comma (map pprBinding asimGs))))

