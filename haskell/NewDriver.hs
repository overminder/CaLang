module NewDriver (
  driverMain,
) where

import Control.Monad.RWS hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.State hiding (forM, forM_, mapM, mapM_, sequence,
                                   sequence_)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable
import Data.Traversable
import Language.Preprocessor.Cpphs
import Prelude hiding (mapM, mapM_, sequence, sequence_, foldr, concat)
import System.IO
import System.Environment
import Text.Dot

import qualified Frontend.AST as F
import qualified Frontend.Parser as F
import qualified Frontend.Rename as F
import qualified Frontend.Simplify as F

import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Simplify as Fg
import qualified Middleend.Tac.Munch as Tac
import qualified Middleend.Tac.Instr as Tac

import Backend.Operand
import qualified Backend.HOST_ARCH.Instr as Arch
import qualified Backend.HOST_ARCH.OptZero.Munch as Arch
import qualified Backend.HOST_ARCH.OptZero.Frame as Arch
import qualified Backend.RegAlloc.Liveness as Ral
--import qualified Backend.RegAlloc.Interference as Ral
import qualified Backend.RegAlloc.IGraph as Ral
--import qualified Backend.RegAlloc.Coalescing as Ral
import qualified Backend.RegAlloc.IterCoalesce as Ral
import qualified Backend.RegAlloc.Coloring as Ral

import Utils.Unique
import Utils.Class
import Utils.OptParse

type DriverT = RWST Option [String] TaskResult

data Option
  = Option {
    opt_input       :: Handle,
    opt_inputFile   :: String,
    opt_output      :: Handle,
    opt_outputFile  :: String,
    opt_outputDescr :: Maybe OutputDescr,
    opt_fuel        :: Int,
    opt_cpp         :: Bool
  }

emptyOption = Option {
  opt_input = stdin,
  opt_inputFile = "<stdin>",
  opt_output = stdout,
  opt_outputFile = "<stdout>",
  opt_outputDescr = Nothing,
  opt_fuel = 0,
  opt_cpp = False
}

type OutputDescr = TaskResult -> DriverT IO ()

data TaskResult
  = TaskResult
  { tr_input               :: String
  , tr_rdrProg             :: F.Program F.Name
  , tr_rnFuncs             :: [F.Func Operand]
  , tr_rnDatas             :: [F.Data Operand]
  , tr_rnExports           :: [F.Name]
  , tr_rnClobRegs          :: [Reg]
  , tr_simFuncs            :: [F.Func Operand]
  , tr_simDatas            :: [F.Data Operand]
  , tr_tacSimGraphs        :: [Fg.FlowGraph Tac.Instr]
  , tr_tacRnDatas          :: [F.Data Operand]
  , tr_natGraphs           :: [Fg.FlowGraph Arch.Instr]
  , tr_natLvGraphs         :: [Fg.FlowGraph (Ral.Liveness Arch.Instr)]
  , tr_ralInterfGraphs     :: [Ral.IGraph]
  , tr_ralCoalLvGraphs     :: [Fg.FlowGraph (Ral.Liveness Arch.Instr)]
  , tr_ralCoalInterfGraphs :: [Ral.IGraph]
  , tr_ralMaps             :: [Map Ral.Vertex Reg]
  , tr_ralGraphs           :: [Fg.FlowGraph (Ral.Liveness Arch.Instr)]
  , tr_platGraphs          :: [Fg.FlowGraph Arch.Instr]
  , tr_gcMaps              :: [[GcMap]]
  , tr_gcMapDatas          :: [F.Data Operand]
  }

emptyTaskResult = TaskResult
  { tr_input               = error "task dep input not resolved yet"
  , tr_rdrProg             = error "task dep rdr not resolved yet"
  , tr_rnFuncs             = error "task dep rn not resolved yet"
  , tr_rnDatas             = error "task dep rn not resolved yet"
  , tr_rnExports           = error "task dep rn not resolved yet"
  , tr_rnClobRegs          = error "task dep rn not resolved yet"
  , tr_simFuncs            = error "task dep sim not resolved yet"
  , tr_simDatas            = error "task dep sim not resolved yet"
  , tr_tacSimGraphs        = error "task dep tac not resolved yet"
  , tr_tacRnDatas          = error "task dep tac not resolved yet"
  , tr_natGraphs           = error "task dep nat not resolved yet"
  , tr_natLvGraphs         = error "task dep nat not resolved yet"
  , tr_ralInterfGraphs     = error "task dep ralIG not resolved yet"
  , tr_ralCoalLvGraphs     = error "task dep ralCoalLv not resolved yet"
  , tr_ralCoalInterfGraphs = error "task dep ralCoalIG not resolved yet"
  , tr_ralMaps             = error "task dep ralMap not resolved yet"
  , tr_ralGraphs           = error "task dep ralG not resolved yet"
  , tr_platGraphs          = error "task dep plat not resolved yet"
  , tr_gcMaps              = error "task dep gc not resolved yet"
  , tr_gcMapDatas          = error "task dep gc not resolved yet"
}

boolOptions = [ ("--rdr-prog",        o printRdrProg)
              , ("--sim",             o printFrontendSim)
              , ("--tac",             o printTacGraph)
              , ("--nat",             o printNatGraph)
              , ("--nat-lv",          o printNatLv)
              , ("--nat-interf",      o printRalInterf)
              , ("--ral-coal-lv",     o printRalCoalLv)
              , ("--ral-coal-interf", o printRalCoalInterf)
              , ("--ral-color",       o printRalAssign)
              , ("--ral",             o printRalGraph)
              , ("--plat",            o printPlatGraph)
              , ("--gc-map",          o printGcMap)
              , ("--gas",             o printGas)
              , ("--cpp",             c True)
              ]
  where
    o wat = \x -> return $ x { opt_outputDescr = Just wat }
    c wat = \x -> return $ x { opt_cpp = wat }

namedOptions = [ ("-o", setOutput)
               , ("--fuel", setFuel)
               ]
  where
    setOutput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path WriteMode
        return $ opt { opt_output = h
                     , opt_outputFile = path
                     }
    setFuel w opt = return $ opt { opt_fuel = read w }

parseDriverOpt asimGs = parseOpt options asimGs emptyOption
  where
    mkBoolOption (key, val) = BoolOption [key] val
    mkNamedOption (key, val) = NamedStringOption [key] val
    options = map mkBoolOption boolOptions ++
              map mkNamedOption namedOptions ++
              [StringOption setInput]

    setInput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path ReadMode
        return $ opt { opt_input = h
                     , opt_inputFile = path
                     }

type Pipeline m = DriverT m ()

runPipeline :: Monad m => [Pipeline m] -> DriverT m ()
runPipeline fs = case fs of
  f:rest -> do
    _ <- f
    runPipeline rest
  _ -> return ()

driverMain :: IO ()
driverMain = do
  args <- getArgs
  opt <- parseDriverOpt args

  -- IO pipeline: do IO related preprocessing
  (ioRes, _) <- execRWST (runPipeline ioPipelines) opt emptyTaskResult

  -- UniqueM: pure compilation runs
  let (result, _) = evalUniqueM (execRWST (runPipeline pipelines) opt ioRes)
      mbOutputDescr = opt_outputDescr opt

  -- IO output: do IO related postprocessing
  case mbOutputDescr of
    Just descr -> do
      execRWST (descr result) opt result
      return ()
    Nothing ->
      return ()

  hClose (opt_output opt)

-- Print helpers

printLine wat = do
  h <- asks opt_output
  liftIO $ hPutStrLn h wat

printFlowGraphs :: Ppr a => [Fg.FlowGraph a] -> DriverT IO ()
printFlowGraphs = printLine . showDot . combineDots . map Fg.graphToDot

printInterfGraphs :: [Ral.IGraph] -> DriverT IO ()
printInterfGraphs = printLine . showDot . combineDots . map Ral.rawIGraphToDot

combineDots = sequence_ . map scope

-- Printers

printRdrProg = printLine . show . F.pprProgram . tr_rdrProg

printFrontendSim = mapM_ (printLine . show . F.pprFunc) . tr_simFuncs

printTacGraph = printFlowGraphs . tr_tacSimGraphs

printNatGraph = printFlowGraphs . tr_natGraphs

printNatLv = printFlowGraphs . tr_natLvGraphs

printRalInterf = printInterfGraphs . tr_ralInterfGraphs

printRalCoalLv = printFlowGraphs . tr_ralCoalLvGraphs

printRalCoalInterf = printInterfGraphs . tr_ralCoalInterfGraphs

printRalAssign result = do
  let dots = map mkDot (zip (tr_ralCoalInterfGraphs result)
                            (tr_ralMaps result))
      mkDot (iGraph, color) = 
        let showVertex v = show $ pprReg (color Map.! v)
         in Ral.iGraphToDot iGraph showVertex
  printLine . showDot . combineDots $ dots

printRalGraph = printFlowGraphs . tr_ralGraphs

printPlatGraph = printFlowGraphs . tr_platGraphs

printGcMap =
  mapM_ (printLine . show . vcat . punctuate comma . map pprGcMap) . tr_gcMaps

printGas result = do
  printAsmExport (tr_rnExports result)
  printAsmData (tr_tacRnDatas result)
  printAsmData (tr_gcMapDatas result)
  printAsmCode (tr_platGraphs result)
  where
    printAsmExport = mapM_ (printLine . ("\t.globl "++))

    printAsmData datas = forM_ datas $ \(F.LiteralData (_, label) lit) -> do
      printLine $ "\t.align 16"
      printLine $ "\t.data"
      printLine $ show (ppr label) ++ ":"
      printLine $ showLit lit

    printAsmCode graphs = do
      forM_ graphs $ \(Fg.MkGraph { Fg.funcName = name
                                  , Fg.blockTrace = bids
                                  , Fg.blockMap = bmap
                                  }) -> do
        printLine $ "\t.p2align 4,,15"
        printLine $ "\t.text"
        printLine $ "\t.type " ++ name ++ ",@function"
        printLine $ name ++ ":"
        forM_ bids $ \bid -> do
          printLine $ show (pprImm (BlockLabel bid)) ++ ":"
          mapM_ (printLine . ("\t"++) . show . ppr) (bmap Map.! bid)
        printLine $ "\t.size " ++ name ++ " ,.-" ++ name

    showLit lit = case lit of
      F.LInt i -> "\t.quad " ++ show i
      F.LChr c -> "\t.byte " ++ show c
      F.LStr s -> "\t.string " ++ show s
      F.LFlo d -> "\t.double " ++ show d
      F.LSym (OpImm s) -> "\t.quad " ++ show (pprImm s)
      F.LArr lits -> unlines (map showLit lits)

ioPipelines :: [Pipeline IO]
ioPipelines = [do h <- asks opt_input
                  src <- liftIO $ hGetContents h
                  modify $ \st -> st { tr_input = src },

               do wantCpp <- asks opt_cpp
                  src <- gets tr_input
                  if wantCpp
                    then let cppOption = defaultBoolOptions { ansi = True
                                                            , locations = False
                                                            }
                          in do
                            inputPath <- asks opt_inputFile
                            cppd' <- liftIO $ cppIfdef inputPath [] ["."]
                                              cppOption src
                            src' <- liftIO $ macroPass [] cppOption cppd'
                            modify $ \st -> st { tr_input = src' }
                    else return ()
              ]


-- Compilation pipeline
pipelines :: [Pipeline UniqueM]
pipelines = [ do src <- gets tr_input
                 -- Frontend/Parse: read input and generate AST
                 modify $ \st -> st { tr_rdrProg = F.readProgram src }

            , do rdrProg <- gets tr_rdrProg
                 (rnDatas, rnFuncs, exports, clobRegs) <- F.rename rdrProg
                 -- Frontend/Rename: Resolve identifier and bindings
                 -- Also check for undefined names.
                 modify $ \st -> st {
                   tr_rnDatas = rnDatas,
                   tr_rnFuncs = rnFuncs,
                   tr_rnExports = exports,
                   tr_rnClobRegs = clobRegs
                 }

            , do rnDatas <- gets tr_rnDatas
                 rnFuncs <- gets tr_rnFuncs
                 (simDatas, simFuncs) <- F.simplify rnDatas rnFuncs
                 -- Frontend/Simplify: lift literal data etc.
                 modify $ \st -> st {
                   tr_simDatas = simDatas,
                   tr_simFuncs = simFuncs
                 }

            , do simFuncs <- gets tr_simFuncs
                 simDatas <- gets tr_simDatas
                 tacGraphs <- mapM mkTacGraph simFuncs
                 -- Middleend/Tac: Generate Tac instr from AST
                 -- Middleend/Graph: Build flowgraph from Tac stream
                 -- Also rename local labels to basic block labels.
                 let simGraphs = map Fg.simplify tacGraphs
                     label2BlockId = foldr (Map.union . Fg.labelMap)
                                           Map.empty tacGraphs
                     renamedDatas = map (renameFlowData label2BlockId) simDatas
                 modify $ \st -> st {
                   tr_tacSimGraphs = simGraphs,
                   tr_tacRnDatas = renamedDatas
                 }

            , do fuel <- asks opt_fuel
                 tacGraphs <- gets tr_tacSimGraphs 
                     -- Backend/Instruction selection: Tac.Instr->Arch.Instr
                 let natGraphs = map Arch.munchGraph tacGraphs

                     -- Backend/RegAlloc: calculate liveness
                     lvGraphs = map (Ral.iterDCE . Ral.iterLiveness) natGraphs

                 -- RegAlloc: build interference graph
                 interfGs <- mapM Ral.buildIGraph lvGraphs

                 -- RegAlloc: recklessly coalesce reg-reg moves
                 -- XXX Disable for now
                 (coalLvGs, coalInterfGs) <- flip evalStateT fuel $ do
                   return (lvGraphs, interfGs)
                   coalGraphPairs <- zipWithM Ral.coalesce lvGraphs interfGs
                   return (unzip coalGraphPairs)

                 -- RegAlloc: select physical reg for each virtual reg
                 clobs <- gets tr_rnClobRegs
                 let usableRegs = generalRegs List.\\ clobs
                     ralMaps = map (Ral.allocPhysReg usableRegs) coalInterfGs
                     ralGs = zipWith3 Ral.assignPhysReg ralMaps
                                                        coalInterfGs
                                                        coalLvGs
                 -- Backend/Platdep: generate prolog/epilog/caller-save etc
                 -- Backend/GC: generate gcmap for each call site
                 (platGs, gcMaps) <- liftM unzip $ do
                   let usedRegs = map (Set.fromList . Map.elems) ralMaps
                   sequence (List.zipWith4 Arch.evalFrameT
                                           ralGs
                                           (repeat clobs)
                                           usedRegs
                                           (repeat Arch.genPlatDepCode))

                 -- Backend/GC: format gcmap as static data
                 gcMapDatas <- genDataFromGcMaps gcMaps

                 modify $ \st -> st {
                   tr_natGraphs = natGraphs,
                   tr_natLvGraphs = lvGraphs,
                   tr_ralInterfGraphs = interfGs,
                   tr_ralCoalLvGraphs = coalLvGs,
                   tr_ralCoalInterfGraphs = coalInterfGs,
                   tr_ralMaps = ralMaps,
                   tr_platGraphs = platGs,
                   tr_gcMaps = gcMaps,
                   tr_gcMapDatas = gcMapDatas
                 }

            ]

renameFlowData dct (F.LiteralData bd lit)
  = F.LiteralData bd (renameFlowLiteral dct lit)

renameFlowLiteral dct lit = case lit of
  F.LSym (OpImm imm) -> F.LSym (OpImm (renameFlowImm dct imm))
  F.LArr xs -> F.LArr (map (renameFlowLiteral dct) xs)
  _ -> lit

renameFlowImm dct imm = case Map.lookup imm dct of
  Nothing -> imm
  Just bid -> BlockLabel bid

mkTacGraph :: F.Func Operand -> DriverT UniqueM (Fg.FlowGraph Tac.Instr)
mkTacGraph (F.Func (OpImm (NamedLabel name)) args body isC) = do
  insns <- Tac.munch body
  let regArgs = map (unReg . snd) args
  Fg.runGraphBuilderT name regArgs isC . Fg.buildGraph $ insns

genDataFromGcMaps gcMaps = do
  let rootName = "CaLang_GcMapRoot"
      mkConcreteGcMap (GcMap retAddr saves escapes fp ptrs) = do
        label <- newTempLabel "GcMap"
        return $ F.LiteralData (F.i64, OpImm label)
                             (F.LArr $ [F.LSym (OpImm retAddr)] ++
                                     map F.LInt (mkPrologSave saves) ++
                                     [F.LInt (mkEscapeBitmap escapes)] ++
                                     [F.LInt (fromIntegral fp)] ++
                                     [F.LInt (fromIntegral . length $ ptrs)] ++
                                     map (F.LInt . fromIntegral) ptrs)
      mkPrologSave saves = let saveMap = Map.fromList saves
                            in map (\r -> fromIntegral
                                     (Map.findWithDefault 0 r saveMap))
                                   Arch.kCalleeSaves
      mkEscapeBitmap escapes = 157

  gcMapDatas <- mapM mkConcreteGcMap (concat gcMaps)
  let rootMap = F.LiteralData (F.i64, OpImm (NamedLabel rootName))
                            (F.LArr $
                              [F.LInt (fromIntegral (length gcMapDatas))] ++
                                    map getLabel gcMapDatas)
      getLabel (F.LiteralData (_, label) _) = F.LSym label

  return (rootMap:gcMapDatas)

