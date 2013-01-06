module Utils.OptParse (
  OptDescr(..),
  parseOpt
) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data OptDescr a
  = BoolOption [String] (a -> IO a)
  | NamedStringOption [String] (String -> a -> IO a)
  | StringOption (String -> a -> IO a)

data OptParser a
  = NoArg (a -> IO a)
  | NeedArg (String -> a -> IO a)

mkParser descrs = flip evalState (Map.empty, Nothing) $ do
  forM_ descrs $ \descr -> case descr of
    BoolOption names func -> forM_ names $ \name -> do
      modify (setFst (Map.insert name (NoArg func)))
    NamedStringOption names func -> forM_ names $ \name -> do
      modify (setFst (Map.insert name (NeedArg func)))
    StringOption func -> do
      modify (setSnd (const (Just func)))
  (dct, mbStrOpt) <- get
  return $ parse dct (mkStrOptHandler mbStrOpt)
  where
    setFst f (x, y) = (f x, y)
    setSnd f (x, y) = (x, f y)
    mkStrOptHandler mbStrOpt = case mbStrOpt of
      Just f -> f
      Nothing -> \x ->
        error $ "OptParse: no string option is accepted but got " ++ show x
    parse dct onStrOpt args = case args of
      [] -> return
      flag@('-':_):restArgs -> case Map.lookup flag dct of
        Nothing -> error $ "OptParse: unknown option: " ++ flag
        Just p -> case p of
          NoArg f -> \m -> f m >>= parse' restArgs
          NeedArg f -> case restArgs of
            x:xs -> \m -> f x m >>= parse' xs
            _ -> error $ "OptParse: no argument for option: " ++ flag
      flag:restArgs -> \m -> onStrOpt flag m >>= parse' restArgs
      where parse' = parse dct onStrOpt

parseOpt :: [OptDescr a] -> [String] -> a -> IO a
parseOpt descrs = mkParser (descrs ++ [BoolOption ["-h", "--help"] help])
  where
    help m = do
      putStrLn $ "Available options:"
      forM_ descrs $ \descr -> do
        case descr of
          BoolOption names _ -> do
            putStrLn (unwords names)
          NamedStringOption names _ -> do
            putStrLn (unwords (map (++" STRING") names))
          StringOption _ -> do
            putStrLn "And positional options."
      return m

