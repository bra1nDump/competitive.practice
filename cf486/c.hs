module Main where

import Data.List as L
import Data.Map as M
import Control.Monad
import Data.ByteString.Char8 as C
import Data.ByteString as BS

readInts :: Int -> ByteString -> ([Int],ByteString)
readInts n bs =
  L.foldl (\(ints,bs') _ -> let
              bs'' = C.dropWhile (\x -> x == ' ' || x == '\n') bs'
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
      {-# SCC sequences' #-}
      mutations = L.map (\(i,seqq) ->
                           let sum = L.sum seqq
                               seqEnum = L.zip [1..(L.length seqq)] seqq
                           in [(sum - x,i,ix) | (ix,x) <- seqEnum]) sequences'
      {-# SCC mutations #-}
      orderedMutations = sortBy (\(sum,_,_) (sum',_,_) -> compare sum sum') $ Prelude.concat mutations
      {-# SCC orderedMutations #-}
      groupedMatches = L.groupBy (\(sum,i,_) (sum',i',_) -> sum == sum' && i /= i') orderedMutations
      {-# SCC groupedMatches #-}
      matches = L.filter (\xs -> L.length xs >= 2) groupedMatches
  if L.null matches then
    Prelude.putStrLn "NO"
  else do
    Prelude.putStrLn "YES"
    let matches' = L.head matches
        (i,j) = L.foldl (\(majorI,minorI) (_,mI,miI) -> if mI `L.elem` majorI then
                                                          (majorI,minorI)
                                                        else (mI:majorI,miI:minorI)) ([],[]) matches'
    forM_ (L.zip i j) (\(ix,i) -> Prelude.putStrLn $ show ix ++ " " ++ show i)
