module Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as C

import qualified Data.Set as S

import Control.Monad

input :: IO BS.ByteString
input = do
  handle <- return stdin --openFile "test.txt" ReadMode
  BS.hGetContents handle

readInts :: Int -> BS.ByteString -> ([Int],BS.ByteString)
readInts n bs = let
  (xs,bs') = foldl (\(xs,bs) eI -> let
                       Just(x,bs') = C8.readInt $ C8.dropWhile C.isSpace bs
                       in (x:xs,bs'))
                ([],bs) [1..n]
  in (xs,bs')

-- # Input # --

powersOf2 = S.fromList $ take 31 . iterate (*2) $ 1

validPairs :: [Int] -> [(Int,Int)]
validPairs xs = let
  in do x1 <- xs
        x2 <- xs
        if S.member (x1-x2) powersOf2 then
          return (x1,x2)
        else
          mzero

main :: IO ()
main = do
  bs <- input
  let ([n],bs') = readInts 1 bs
      (xs,_) = readInts n bs'
      cliques2 = validPairs xs
      cliques3 = [[x1,x2,x3] |
                   (x1,x2) <- cliques2,
                   x3 <- xs,
                   S.member (abs (x1 - x3)) powersOf2,
                   S.member (abs (x2 - x3)) powersOf2
                             ]
  if not . null $ cliques3 then do
    print 3
    forM_ (head cliques3) (\x -> putStr $ show x ++ " ")
  else if not . null $ cliques2 then do
    print 2
    putStrLn $ show (fst $ head cliques2) ++ " " ++ show (snd $ head cliques2)
  else do print 1
          print (head xs)
