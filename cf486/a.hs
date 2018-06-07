module Main where

import Data.List
import Control.Monad

getInts :: IO [Int]
getInts = fmap (map read . words) getLine

main :: IO ()
main = do
  [n,k] <- getInts
  ratings <- getInts
  let sorted = sortBy (\(_,r) (_,r') -> compare r r') $ zip [1..n] ratings
      removedDuplicates = groupBy (\(_,r) (_,r') -> r == r') sorted
  if length removedDuplicates >= k then do
    putStrLn "YES"
    forM_ (take k removedDuplicates) (\((i,_):_) -> putStr $ show i ++ " ")
  else
    putStrLn "NO"
