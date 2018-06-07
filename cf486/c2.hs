module Main where

import Data.List as L
import Data.Map as M
import Control.Monad
import Data.ByteString.Char8 as C
import Data.ByteString as BS

readInts :: Int -> ByteString -> ([Int],ByteString)
readInts n bs =
  L.foldl (\(ints,bs') _ -> let
              bs'' = C.dropWhile (\x -> x == ' ' || x == '\r' || x == '\n') bs'
              Just (int,bs''') = C.readInt bs''
              in (int:ints,bs''')) ([],bs) [1..n]

main :: IO ()
main = do
  n <- fmap read Prelude.getLine
  input <- BS.getContents
  let (sequences,_) = L.foldl (\(xs, bs) _ ->
                                 let ([si],bs') = readInts 1 bs
                                     (xi,bs'') = readInts si bs'
                                 in (xi:xs,bs'')) ([],input) [1..n]
      sequences' = L.zip [1..n] $ L.reverse $ L.map L.reverse sequences
      mutations = L.map (\(i,seqq) ->
                           let sum = L.sum seqq
                               seqEnum = L.zip [1..(L.length seqq)] seqq
                           in [(sum - x,i,ix) | (ix,x) <- seqEnum]) sequences'
      orderedBySum = sortBy (\(sum,_,_) (sum',_,_) -> compare sum sum') $ Prelude.concat mutations
      groupedMatches = L.groupBy (\(sum,i,_) (sum',i',_) -> sum == sum' && i == i') orderedBySum
      noDuplicates = L.map L.head groupedMatches
      finalGrouping = L.groupBy (\(sum,_,_) (sum',_,_) -> sum == sum') noDuplicates
      matches = L.filter ((<=) 2 . L.length) finalGrouping
  if L.null matches then
    Prelude.putStrLn "NO"
  else do
    Prelude.putStrLn "YES"
    let match = L.take 2 . L.head $ matches
    forM_ match (\(_,ii,i) -> Prelude.putStrLn $ show ii ++ " " ++ show i)
