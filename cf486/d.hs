module Main where

{-# LANGUAGE ScopedTypeVariables #-}

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as C

import qualified Data.Array as A
import Data.Array.ST
import Data.Array.MArray
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Control.Monad
import Control.Monad.ST
import Control.Monad.State

input :: IO BS.ByteString
input = do
  handle <- openFile "test.txt" ReadMode
  BS.hGetContents handle

readInts :: Int -> BS.ByteString -> ([Int],BS.ByteString)
readInts n bs = let
  (xs,bs') = foldl (\(xs,bs) eI -> let
                       Just(x,bs') = C8.readInt $ C8.dropWhile C.isSpace bs
                       in (x:xs,bs'))
                ([],bs) [1..n]
  in (reverse xs,bs')

type Table a = A.Array Int a
type STTable s a = STArray s Int a
type Vertice = Int
type Edge = (Vertice,Vertice)
type Graph = Table [Vertice]

readGraph :: BS.ByteString -> (Int,[Edge],BS.ByteString)
readGraph bs =
  let ([v,e],bs') = readInts 2 bs
      (edges,bs'') = foldl (\(edges,bs) e -> let
                               ([from,to],bs') = readInts 2 bs
                               in ((from,to):edges,bs'))
                     ([],bs') [1..e]
  in (v,edges,bs'')

addEdge :: (Int,Int) -> STTable s [Vertice] -> (ST s) ()
addEdge (from,to) graph = do
  neighbours <- readArray graph from
  writeArray graph from (to:neighbours)

fromEdges :: Int -> [Edge] -> Graph
fromEdges verticeCount edges =
  runSTArray $ do
    emptyGraph <- newArray (1,verticeCount) []
    foldM (\graph (from,to) -> do
              addEdge (from,to) graph
              addEdge (to,from) graph
              return graph)
      emptyGraph edges

dfs :: Vertice -> Graph -> Graph
dfs from graph = runSTArray $ do
  discovered <- newArray (A.bounds graph) False
  dfsTree <- newArray (A.bounds graph) []
  dfs' [from] discovered dfsTree
    where dfs' :: [Vertice] -> STTable s Bool -> STTable s [Vertice] -> ST s (STTable s [Vertice])
          dfs' [] _ dfsTree = return dfsTree
          dfs' (v:vs) discovered dfsTree = do
            writeArray discovered v True
            let neighbours = graph A.! v
            notDiscovered <- filterM
              (\n -> readArray discovered n >>= (return . not))
              neighbours
            forM_ notDiscovered (\n -> addEdge (v,n) dfsTree)
            dfs' (notDiscovered ++ vs) discovered dfsTree

-- BronKerbosh algorithm
data BKState = BKState {
    r :: S.Set Vertice
    , p :: S.Set Vertice
    , x :: S.Set Vertice
    }

bkStateInit :: [Vertice] -> BKState
bkStateInit p = BKState {
  r = S.empty
  , p = S.fromList p
  , x = S.empty
  }

maximalCliques :: Graph -> [S.Set Vertice]
maximalCliques graph = let
  (start,end) = A.bounds graph
  in bk $ bkStateInit [start..end]
  where bk state@(BKState {r = r,p = p,x = x}) =
          if S.null p && S.null x then [r]
          else let
            (cliques,_) = foldl (\(cliques,state) v -> let
                                    BKState {r = r,p = p,x = x} = state
                                    neighbours = S.fromList $ graph A.! v
                                    recur = bk $ BKState {r = S.insert v r,
                                                          p = S.intersection p neighbours,
                                                          x = S.intersection x neighbours}
                                    newState = BKState {r = r,
                                                        p = S.delete v p,
                                                        x = S.insert v x}
                                    in (recur:cliques,newState)
                                ) ([],state) p
            in (concat cliques)

maximumClique :: Graph -> S.Set Vertice
maximumClique = L.maximumBy (\s1 s2 -> compare (S.size s1) (S.size s2)) . maximalCliques

-- # Input # --

powersOf2 = takeWhile (< 2 * 1000000000) . iterate (*2) $ 1

validPairs :: [Int] -> [Edge]
validPairs xs = let
  p2Set = S.fromList powersOf2
  xsI = zip xs [1..]
  in do (x1,i1) <- xsI
        (x2,i2) <- xsI
        if S.member (x1-x2) p2Set then
          return (i1,i2)
        else
          mzero

main :: IO ()
main = do
  file <- openFile "test.txt" ReadMode --return stdin
  bs <- BS.hGetContents file
  let ([n],bs') = readInts 1 bs
      (xs,_) = readInts n bs'
      valid = validPairs xs
      graph = fromEdges n valid
      indexes = S.toList $ maximumClique graph
  print xs
  forM_ indexes (\i -> putStr $ show (xs !! i) ++ " ")
