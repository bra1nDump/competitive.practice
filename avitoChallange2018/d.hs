module Main where

import Data.Bits
import Control.Monad.State

maxAnd :: Int -> [Integer] -> Integer -> Integer -> Integer
maxAnd k values currentShelf acc
  | k == 0 && null values = acc
  | k == 0 = 0
  | k > length values = 0
  | otherwise =
    let (value:remaining) = values
        updatedShelf = currentShelf + value
        placeOnCurrent = maxAnd k remaining updatedShelf acc
        changeShelf = maxAnd (k-1) remaining 0 (acc .&. updatedShelf)
    in max placeOnCurrent changeShelf

main :: IO ()
main = do
  [n,k] <- fmap (map read . words) getLine
  values <- fmap (map read . words) getLine :: IO [Integer]
  print $ maxAnd k values 0 (complement zeroBits)

