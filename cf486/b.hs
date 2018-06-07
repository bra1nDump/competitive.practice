module Main where

import Data.List
import Control.Monad

main :: IO ()
main = do
  n <- fmap read getLine
  strings <- sequence $ take n $ repeat getLine
  let sortedByLength = sortBy (\str str' -> compare (length str) (length str')) strings
      grouped = group sortedByLength
      groupedByLength = groupBy (\str str' -> length str == length str') sortedByLength
  if length grouped /= length groupedByLength then do
    putStrLn "NO"
  else do
    let (_,satisfy) = foldl (\(preceeding,flag) current ->
                                 let newFlag = all id $ map (flip isInfixOf current) preceeding
                                 in (current:preceeding,flag && newFlag)) ([],True) $ map head grouped
    if satisfy then do
      putStrLn "YES"
      forM_ (concat grouped) (\str -> putStrLn str) 
    else
      putStrLn "NO"
