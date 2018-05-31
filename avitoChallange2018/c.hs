module Main where

import Data.Array
import Data.Array.ST.Safe
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.List

type Graph = Array Int [Int]

getTree :: IO Graph
getTree = do
  input <- fmap (C.split '\n') $ BS.getContents
  let verticeCount = read . C.unpack . head $ input
      edgesBS = [(v1,v2) | edgeBS <- tail input, not $ BS.null edgeBS, let [v1, v2] = C.split ' ' edgeBS]
      edges = map (\(v1,v2) -> let
                      Just (v1', _) = C.readInt v1
                      Just (v2', _) = C.readInt v2
                      in (v1',v2')) edgesBS
  return $ runSTArray $ do
    tree <- newArray (1,verticeCount) []
    forM_ edges (\(v1,v2) -> do
                    v1Neighbours <- readArray tree v1
                    v2Neighbours <- readArray tree v2
                    writeArray tree v1 (v2:v1Neighbours)
                    writeArray tree v2 (v1:v2Neighbours))
    return tree

main :: IO ()
main = do
  tree <- getTree
  let degrees = fmap (\(id, neighbours) -> (id, length neighbours)) $ assocs tree
      leafs = filter (\(_, d) -> d == 1) degrees
      maxVerticeDegree = maximumBy (\(_, d1) (_,d2) -> compare d1 d2) degrees
      ok = null . filter (\(_,x) -> x /= 1 && x /= 2 && x /= snd maxVerticeDegree) $ degrees
  if (length leafs) == snd maxVerticeDegree && ok || length degrees == 2 then do
    putStrLn "Yes"
    if snd maxVerticeDegree == 2 || length degrees == 2 then do
      putStrLn "1"
      mapM_ (putStr . (++ " ") .  show . fst) leafs
    else do
      print $ length leafs
      forM_ leafs (\(id,_) -> putStrLn $ show (fst maxVerticeDegree) ++ " " ++ show id)
  else
    putStrLn "No"

  
