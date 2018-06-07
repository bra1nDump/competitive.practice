module Main where

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char as C

import Data.Int
import qualified Data.Set as S

import Control.Monad

input :: IO BS.ByteString
input = do
  handle <- openFile "test.txt" ReadMode
  BS.hGetContents handle

readInts :: BS.ByteString -> [Int]
readInts = map parse . C8.words
  where parse s = let Just(x,_) = C8.readInt s in x

-- # Input # --

powersOf2 :: S.Set Int64
powersOf2 = S.fromList $ takeWhile (<= 2 * 1000000000) . iterate (*2) $ 1

validPairs :: [Int64] -> [(Int64,Int64)]
validPairs xs = do
  x1 <- xs
  x2 <- xs
  if isMember (x1-x2) then return (x1,x2)
  else mzero
  where isMember x = S.member x powersOf2
  


main :: IO ()
main = do
  bs <- input
  let (nl:xsl:_) = C8.lines bs
      [n] = readInts nl
      xs' = readInts xsl
      xs = map fromIntegral xs'
      cliques2 = validPairs xs
  putStrLn $ show $ length cliques2
  --let candidates = cliques2 >>= (\(x,y) -> [x,y])
  let cliques3 = [[x1,x2,x3] |
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
  else do
    print 1
    print (head xs)
