module Main where

import Data.Array
import Data.Array.ST.Safe
import Control.Applicative
import Control.Monad
import Control.Monad.ST.Safe
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

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

weights :: Graph -> Array Int Int
weights tree = runSTArray $ do
  let (start,end) = bounds tree
      verticeCount = end - start + 1
  weights <- newArray (1,verticeCount) (-1)
  update tree 1 weights
  return weights
    where
      update :: Graph -> Int -> STArray s Int Int -> ST s Int
      update tree branch weights = do
        writeArray weights branch 0
        children <- filterM (\child -> do
                                weight <- readArray weights child
                                return $ weight == -1) $ tree ! branch -- 7.3 %
        childrenWeights <- mapM (\child -> update tree child weights) children
        let branchWeight = foldl (+) 1 childrenWeights
        writeArray weights branch branchWeight
        return branchWeight

countRemovedEdges :: Graph -> Array Int Int -> Maybe Int
countRemovedEdges tree weights = runST $ do
  let (start,end) = bounds tree
      verticeCount = end - start + 1
  discovered <- newArray (1,verticeCount) False
  remove 1 0 discovered
  where
    remove :: Int -> Int -> STArray s Int Bool -> ST s (Maybe Int)
    remove vertice currentSize discovered = do
      writeArray discovered vertice True
      children <- filterM (\child -> readArray discovered child
                            >>= (\x -> return $ not x)) $ tree ! vertice -- 5.2 %
      let overflows = fmap
            (\child -> (child, (weights ! child) `mod` 2))
            children
          totalOverflows = foldl (\acc (_,overflow) -> acc + overflow) 0 overflows
          edgesRemoved = (length children) - totalOverflows
      if (totalOverflows + currentSize + 1) `mod` 2 /= 0
        then return Nothing
        else foldM (\removedFromChildren (child, overflow) -> do
                       removed <- remove child overflow discovered
                       return $ (+) <$> removedFromChildren <*> removed)
             (Just edgesRemoved) overflows

main :: IO ()
main = do
  tree <- getTree
  case countRemovedEdges tree (weights tree) of
    Just edgesRemoved -> print edgesRemoved
    Nothing -> putStr "-1"
