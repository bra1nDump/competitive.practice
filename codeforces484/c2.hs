module Main (main) where

import Data.Array
import Data.Array.ST
import Data.Array.MArray
import Control.Monad
import Control.Monad.ST
import Control.Monad.State

type Graph = Array Int [Int]

getEdge :: IO (Int, Int)
getEdge = do
  [from, to] <- fmap (map read . words) getLine
  return (from, to)

getTree :: IO Graph
getTree = do
  verticeCount <- fmap read getLine
  edges <- sequence $ take (verticeCount - 1) . repeat $ getEdge
  let bidirectionalEdges = edges ++ map (\(v,v') -> (v',v)) edges
      emptyTree = array (1,verticeCount) [(i, []) | i <- [1..verticeCount]]
      tree = foldr addNeighbour emptyTree bidirectionalEdges
        where addNeighbour (node,neighbour) graph =
                let neighbours = graph ! node
                in graph // [(node, neighbour : neighbours)]
  return tree

branchWeights :: Graph -> Int -> State (Array Int Int) Int
branchWeights tree branch = do
  let allChildren = tree ! branch
  modify (// [(branch, 0)])
  weights <- get
  let children = filter (\child -> weights ! child == -1) allChildren
  childrenWeights <- mapM (\child -> branchWeights tree child) children
  let branchWeight = foldl (+) 1 childrenWeights
  modify (// [(branch, branchWeight)])
  return branchWeight

canSplit :: Graph -> Array Int Int -> Int -> Bool
canSplit tree weights componentSize = runST $ do
  let (start,end) = bounds tree
      verticeCount = end - start + 1
  discovered <- newArray (1,verticeCount) False
  canSplitSubTree 1 0 discovered
  where
    canSplitSubTree :: Int -> Int -> STArray s Int Bool -> ST s Bool
    canSplitSubTree vertice currentSize discovered = do
      let allChildren = tree ! vertice
      writeArray discovered vertice True
      children <- filterM (\child -> readArray discovered child >>= (\x -> return $ not x)) allChildren
      let overflows = fmap
            (\child -> (child, (weights ! child) `mod` componentSize))
            children
          totalOverflows = foldl (\acc (_,overflow) -> acc + overflow) 1 overflows
      if totalOverflows + currentSize /= componentSize
        then return False
        else do
        childrenOK <- forM overflows
          (\(child, overflow) -> do
              let sizeAfterMergingChildren = (componentSize - overflow) `mod` componentSize
              canSplitSubTree child sizeAfterMergingChildren discovered)
        return $ all id childrenOK

evenComponentSizes :: Graph -> [Int]
evenComponentSizes tree =
  let (first,last) = bounds tree
      verticeCount = last - first + 1
      initWeights = array (1,verticeCount) [(i,-1) | i <- [1..verticeCount]]
      weights = snd $ runState (branchWeights tree 1) initWeights
      candidateComponentSizes = [x | x <- [2..(verticeCount `div` 2)], verticeCount `mod` x == 0]
  in filter (\size -> canSplit tree weights size) candidateComponentSizes

main :: IO ()
main = do
  tree <- getTree
  let sizes = evenComponentSizes tree
  if null sizes
    then putStr "-1"
    else do
    let minComponentSize = head sizes
        (first,last) = bounds tree
        verticeCount = last - first + 1
    putStr $ show $ verticeCount `div` minComponentSize - 1
