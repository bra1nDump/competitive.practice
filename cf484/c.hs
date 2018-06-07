-- You're given a tree with n

-- vertices.

-- Your task is to determine the maximum possible number of edges that can be removed in such a way that all the remaining connected components will have even size.
-- Input

-- The first line contains an integer n
-- (1≤n≤105

-- ) denoting the size of the tree.

-- The next n−1
-- lines contain two integers u, v (1≤u,v≤n) each, describing the vertices connected by the i

-- -th edge.

-- It's guaranteed that the given edges form a tree.
-- Output

-- Output a single integer k
-- — the maximum number of edges that can be removed to leave all connected components with even size, or −1 if it is impossible to remove edges in order to satisfy this property.

module Main (main) where

import Data.List (find)

readEdge :: IO (Int,Int)
readEdge = do
  [v1, v2] <- fmap ( map read . words) getLine
  return (v1, v2)

readEdges :: Int -> IO [(Int, Int)]
readEdges e = sequence . take e . repeat $ readEdge

toComponents :: Int -> Int -> Int -> [(Int, (Int, Int))] -> Bool
toComponents size currentSize vertice edges =
  let children = map (\(_, (v2, weight)) -> (v2, weight `mod` size))
        . filter (\(v1, _) -> v1 == vertice) $ edges
      overflow = foldl (\a (_, w) -> a + w) 0 children
      newEdges = filter (\(_, (v,_)) -> v /= vertice) edges
      descendendOk = all (== True)
        $ fmap (\(child, overflow) ->
                  toComponents size (size - overflow) child newEdges) children 
  in  (size - currentSize - 1) == overflow && descendendOk

weighted :: Int -> [(Int, Int)] -> ([(Int, (Int, Int))], Int)
weighted vertice edges =
  let children = filter (\(v1, v2) -> v1 == vertice) edges
      newEdges = filter (\(v,_) -> v /= vertice) edges
  in foldl (\(tree, totalWeight) ((subTree, subWeight), child) -> (tree ++ subTree ++ [(vertice, (child, subWeight))], totalWeight + subWeight)) ([],1)
     $ fmap (\(_, child) -> (weighted child newEdges, child)) children

maxEvenComponents :: [(Int, Int)] -> Int
maxEvenComponents edges =
  let (weigtedEdges, totalWeight) = weighted 1 edges
      verticeCount = length edges
      possibleFamilySizes = filter (\size -> verticeCount `mod` size == 0) [2..(verticeCount `div` 2)]
      result = find (\(size, ok) -> ok) $
        zip possibleFamilySizes $ fmap (\n -> toComponents n 0 1 weigtedEdges) possibleFamilySizes
  in case result of
    Nothing -> -1
    (Just(size,_)) -> verticeCount `div` size

main :: IO ()
main = do
  v <- fmap read getLine
  edges <- readEdges (v - 1)
  let biDirectionalEdges = edges ++ (fmap (\(v1,v2) -> (v2,v1)) edges)
  print $ maxEvenComponents biDirectionalEdges
  
