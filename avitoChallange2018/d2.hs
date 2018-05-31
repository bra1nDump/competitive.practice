module Main where

import qualified Data.Map as M
import qualified Data.Array as A
import Data.Bits
import Data.List
import Control.Monad.State

main :: IO ()
main = do
  [n,k] <- fmap (map read . words) getLine
  values <- fmap (map read . words) getLine :: IO [Integer]
  let shelfs = A.array ((1,1),(n,n)) [((i,j), shelf) |
                                       i <- [1..n], j <- [1..n]
                                       , let shelf =
                                               if i <= j then sum $ take (j - i + 1) . drop (i - 1) $ values
                                               else 0]
      memInit = A.array ((0,0),(n,k)) [((n',k'),(p,[])) |
                                        n' <- [0..n], k' <- [0..k]
                                        , let p = if k' == 0 && n' == 0 then
                                                    complement zeroBits
                                                  else if k' == n' then
                                                    foldl1 (.&.) $ take n' values
                                                  else if k' > n' then
                                                    0
                                                  else -100
                                        ]
      solutionSpace = sortBy (\(_,k1) (_,k2) -> compare k1 k2) [(j,k') | j <- [1..n], k' <- [1..k]]
      solutions = foldl updateOptimum memInit solutionSpace
        where updateOptimum mem (j,k') = 
                let solution = maximum [shelfs A.! (i,j) .&. (fst $ mem A.! (i-1,k'-1)) | i <- [1..j]]
                    
                in mem A.// [((j,k'),(solution)]
  print solutionSpace
  print solutions
  print $ show $ solutions A.! (n,k)
