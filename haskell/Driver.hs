module Driver (
  driverMain
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import qualified Data.Foldable as Foldable
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import System.IO
import System.Environment
import Text.Dot
import Text.PrettyPrint
import Language.Preprocessor.Cpphs

import Frontend.AST
import Frontend.Parser
import Frontend.Rename
import qualified Frontend.Simplify as Sim
import qualified Middleend.FlowGraph.Builder as Fg
import qualified Middleend.FlowGraph.Simplify as Fg
import qualified Middleend.Tac.Munch as Tac
import qualified Middleend.Tac.Instr as Tac
--import qualified Middleend.Tac.LocalOpt as Tac
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
    optInputFile :: String,
    optOutput :: Handle,
    optOutputFile :: String,
    optOutputLevel :: OutputOpt,
    runCpp :: Bool
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
  | OutputOptZeroPlatGraph
  | OutputOptZeroGcMap
  | OutputOptZero

parseDriverOpt asimGs = parseOpt options asimGs emptyOption
  where
    emptyOption = Option stdin "<stdin>" stdout "<stdout>" OutputNothing False
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
              , BoolOption ["--O0-plat"] (setOutputLevel OutputOptZeroPlatGraph)
              , BoolOption ["--O0-gcmap"] (setOutputLevel OutputOptZeroGcMap)
              , BoolOption ["--O0"] (setOutputLevel OutputOptZero)
              , BoolOption ["--cpp"] (\x -> return $ x { runCpp = True })
              , NamedStringOption ["-o"] setOutput
              , StringOption setInput
              ]
    setInput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path ReadMode
        return $ opt { optInput = h
                     , optInputFile = path
                     }
    setOutput path opt = case path of
      "-" -> return opt
      _ -> do
        h <- openFile path WriteMode
        return $ opt { optOutput = h
                     , optOutputFile = path
                     }

runAllPasses s = evalUniqueM $ do
  let rdrProg = runParsePass s
  frRes <- runFrontendPass rdrProg
  grRes <- runGraphPass frRes
  oOs <- runOptZeroPass frRes grRes
  return (frRes, grRes, oOs)

driverMain :: IO ()
driverMain = do
  args <- getArgs
  opt <- parseDriverOpt args
  flip runReaderT opt $ do
    h <- asks optInput
    src <- lift $ hGetContents h
    wantCpp <- asks runCpp
    inputPath <- asks optInputFile
    cppSrc <- if wantCpp
                then let cppOption = defaultBoolOptions { ansi = True
                                                        , locations = False
                                                        }
                      in do
                        cppd' <- lift $ cppIfdef inputPath [] ["."]
                                        cppOption src
                        lift $ macroPass [] cppOption cppd'
                else return src
    --lift $ hPutStr stderr cppSrc
    let result = runAllPasses cppSrc
    outputPass result
    h <- asks optOutput
    lift $ hClose h

outputPass ( (prog, (exports, clobs, _), fs)
           , (iss, simDatas, simGs)
           , (natGs, lvNatGs, interfGs, assignMaps, ralGs, platNatGs,
              rawGcMaps, gcMapDatas)) = do
  h <- asks optOutput
  level <- asks optOutputLevel
  case level of
    OutputNothing -> return ()
    OutputRdrProg -> lift . hPutStrLn h . show . pprProgram $ prog
    OutputFrSim -> mapM_ (lift . hPutStrLn h . show . pprFunc) fs
    OutputRawInstr -> do
      forM_ (zip fs iss) $ \(f, is) -> do
        lift . hPutStrLn h $ "# Entry for <" ++ show (pprSignature f) ++ ">"
        mapM_ (lift . hPutStrLn h . show . ppr) is
    OutputRawDot -> output_flowGraphs simGs
    --OutputLocOptDot -> output_flowGraphs locOptGs
    OutputOptZeroDot -> output_flowGraphs natGs
    OutputOptZeroLiveness -> output_flowGraphs lvNatGs
    OutputOptZeroInterf -> output_interfGraphs interfGs
    OutputOptZeroColor -> do
      let dots = map mkDot (zip interfGs assignMaps)
          mkDot (graph, color) = 
            let showVertex v = show $ pprReg (color Map.! v)
             in Ral.graphToDot graph showVertex
      lift . hPutStrLn h . showDot . combine_dots $ dots
    OutputOptZeroRegAlloc -> output_flowGraphs ralGs
    OutputOptZeroPlatGraph -> output_flowGraphs platNatGs
    OutputOptZeroGcMap -> output_gcMap rawGcMaps
    OutputOptZero -> do
      output_asm_export exports
      output_asm_data simDatas
      output_asm_data gcMapDatas
      output_asm_code platNatGs

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

    output_asm_export exports = do
      h <- asks optOutput
      forM_ exports $ \s -> do
        lift . hPutStrLn h $ "\t.globl " ++ s

    output_asm_data datas = do
      h <- asks optOutput
      forM_ datas $ \(LiteralData (_, label) lit) -> do
        lift . hPutStrLn h $ "\t.align 16"
        lift . hPutStrLn h $ "\t.data"
        lift . hPutStrLn h $ show (ppr label) ++ ":"
        lift . hPutStrLn h . show_lit $ lit

    show_lit lit = case lit of
      LInt i -> "\t.quad " ++ show i
      LChr c -> "\t.byte " ++ show c
      LStr s -> "\t.string " ++ show s
      LFlo d -> "\t.double " ++ show d
      LSym (OpImm s) -> "\t.quad " ++ show (pprImm s)
      LArr lits -> unlines (map show_lit lits)

    output_gcMap gcMaps = do
      h <- asks optOutput
      forM_ gcMaps $ \gcmap -> do
        lift $
          hPutStrLn h . show . vcat . punctuate comma . map pprGcMap $
            gcmap

    output_asm_code graphs = do
      h <- asks optOutput
      let writeLn = lift . hPutStrLn h
      forM_ graphs $ \g -> do
        let name = Fg.funcName g
        writeLn $ "\t.p2align 4,,15"
        writeLn $ "\t.text"
        writeLn $ "\t.type " ++ name ++ ",@function"
        writeLn $ name ++ ":"
        let blocks = getBlockTrace (Fg.blockTrace g) (Fg.blockMap g)
        forM_ blocks $ \b -> do
          writeLn $ show (pprImm (BlockLabel (Fg.blockId b))) ++ ":"
          Foldable.mapM_ (writeLn . ("\t"++) . show . ppr) b
        writeLn $ "\t.size " ++ name ++ " ,.-" ++ name
      where
        getBlockTrace ids bmap = map (bmap Map.!) ids



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

runGraphPass (_, (_, _, datas), simFuncs) = do
  (insnss, rawGraphs) <- liftM unzip $ mapM mkInstrAndGraph simFuncs
  let simGraphs = map Fg.simplify rawGraphs
  --locOptGrs <- mapM Tac.runOpt simGraphs

  -- Since we renamed local labels to block labels during flow graph
  -- building, we need to rename static datas that refer to local labels
  -- as well.
  let symbolRenamer = foldr (Map.union . Fg.labelMap) Map.empty rawGraphs
      renamedDatas = map (renameData symbolRenamer) datas
  return (insnss, renamedDatas, simGraphs)

  where
  mkInstrAndGraph (Func (OpImm (NamedLabel name)) asimGs body isC) = do
    insns <- Tac.runMunchM . Tac.munch $ body
    let regAsimGs = map (unReg . snd) asimGs
    graph <- Fg.runGraphBuilderM name regAsimGs isC . Fg.buildGraph $ insns
    return (insns, graph)

  renameImm dct imm = case Map.lookup imm dct of
    Nothing -> imm
    Just bid -> BlockLabel bid
  renameLiteral dct lit = case lit of
    LSym (OpImm imm) -> LSym (OpImm (renameImm dct imm))
    LArr xs -> LArr (map (renameLiteral dct) xs)
    _ -> lit
  renameData dct (LiteralData bd lit)
    = LiteralData bd (renameLiteral dct lit)
  unReg (OpReg r) = r

runOptZeroPass (_, (_, clobs, _), _) (_, _, simGs) = do
  let natGs = map (Arch.evalMunchM . Arch.munchGraph) simGs
      lvNatGs = map (Ral.iterDCE . Ral.iterLiveness) natGs
  interfGs <- mapM Ral.buildGraph lvNatGs
  let assignMaps = map (Ral.allocPhysReg (reverse (generalRegs List.\\ clobs)))
                   interfGs
      ralGs = zipWith3 Ral.assignPhysReg assignMaps interfGs lvNatGs
  (platNatGs, gcMaps) <- liftM unzip $ sequence (List.zipWith4
                                    Arch.evalFrameM
                                    ralGs
                                    (repeat clobs)
                                    (map (Set.fromList . Map.elems) assignMaps)
                                    (repeat Arch.genPlatDepCode))
  
  let rootName = "CaLang_GcMapRoot"
      mkConcreteGcMap (GcMap retAddr saves escapes fp ptrs) = do
        label <- newTempLabel "GcMap"
        return $ LiteralData (i64, OpImm label)
                             (LArr $ [LSym (OpImm retAddr)] ++
                                     map LInt (mkPrologSave saves) ++
                                     [LInt (mkEscapeBitmap escapes)] ++
                                     [LInt (fromIntegral fp)] ++
                                     [LInt (fromIntegral . length $ ptrs)] ++
                                     map (LInt . fromIntegral) ptrs)
      mkPrologSave saves = let saveMap = Map.fromList saves
                            in map (\r -> fromIntegral
                                     (Map.findWithDefault 0 r saveMap))
                                   Arch.kCalleeSaves
      mkEscapeBitmap escapes = 0

  gcMapDatas <- mapM mkConcreteGcMap (concat gcMaps)
  let rootMap = LiteralData (i64, OpImm (NamedLabel rootName))
                            (LArr $ [LInt (fromIntegral (length gcMapDatas))] ++
                                    map getLabel gcMapDatas)
      getLabel (LiteralData (_, label) _) = LSym label

  return (natGs, lvNatGs, interfGs, assignMaps, ralGs, platNatGs,
          gcMaps, rootMap:gcMapDatas)

pprSignature (Func name asimGs _ _)
  = ppr name <> (parens (hcat (punctuate comma (map pprBinding asimGs))))

