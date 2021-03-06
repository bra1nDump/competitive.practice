module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.HashTable.ST.Basic as HT
import Data.HashTable.Class
import Control.Monad.ST

readInts :: BS.ByteString -> [Int]
readInts bStr =
  let list = C.split ' ' bStr
  in [int | bInt <- list, not . C.null $ bInt, let Just (int,_) = C.readInt bInt]

minCost :: A.Array Int Int -> A.Array Int Int -> Int -> Int
minCost fonts costs n =
  runST $ do
    table <- HT.new
    opt n maxBound 3 table
    where opt :: Int -> Int -> Int -> (HT.HashTable s (Int,Int,Int) Int) -> ST s Int
          --     index  upperbound toPlace
          opt _ _ 0 _ = return 0
          opt 0 _ _ _ = return (-1)
          opt _ 0 _ _ = return (-1)
          opt j bound toPlace table = do
            mem <- HT.lookup table (j,bound,toPlace)
            -- when (mem /= Nothing) (let (Just price) = mem in return price)
            -- guard (mem == Nothing)
            case mem of
              (Just price) -> return price
              Nothing -> do
                let costJ = costs A.! j
                    fontJ = fonts A.! j
                if fontJ >= bound then do
                  opt (j-1) bound toPlace table
                  else do
                  memorized1 <- opt (j-1) fontJ (toPlace - 1) table
                  memorized2 <- opt (j-1) bound toPlace table
                  let minCost = customMin costJ memorized1 memorized2
                  HT.insert table (j,if minCost == memorized1 then fontJ else bound,toPlace) minCost
                  return minCost
          customMin :: Int -> Int -> Int -> Int
          customMin v1' v1 v2 =
            case (v1 == -1, v2 == -1) of
              (True,True) -> -1
              (False,True) -> v1 + v1'
              (True,False) -> v2
              (False,False) -> min (v1 + v1') v2

main :: IO ()
main = do
  input <- BS.getContents
  let lines = take 3 . C.split '\n' $ input
      Just (n,_) = C.readInt $ lines !! 0
      fonts = readInts $ lines !! 1
      costs = readInts $ lines !! 2
      cost = minCost (A.array (1,n) (zip [1..n] fonts)) (A.array (1,n) (zip [1..n] costs)) n
  print cost
