module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.Map as M

import Control.Monad.State

readInts :: BS.ByteString -> [Int]
readInts bStr =
  let list = C.split ' ' bStr
  in [int | bInt <- list, not . C.null $ bInt, let Just (int,_) = C.readInt bInt]

minCost :: A.Array Int Int -> A.Array Int Int -> Int -> Int
minCost fonts costs n =
  let (r,st) = runState (opt n Nothing 3) M.empty
        where opt :: Int -> Maybe Int -> Int -> State (M.Map (Int, Maybe Int, Int) Int) Int
              -- index upperbound toPlace
              opt j fontBound 0 = do
                return 0
              opt 0 fontBound toPlace = do
                return (-1)
              opt j (Just 0) toPlace = do
                return (-1)
              opt j fontBound toPlace = do
                memorized <- gets $ M.lookup (j,fontBound,toPlace)
                case memorized of
                  Just cost -> return cost
                  Nothing -> do
                    let costJ = costs A.! j
                        fontJ = fonts A.! j
                    case fontBound of
                      Nothing -> do
                        memorized1 <- opt (j-1) (Just fontJ) (toPlace - 1)
                        memorized2 <- opt (j-1) Nothing toPlace
                        let minCost = customMin costJ memorized1 memorized2
                        modify $ M.insert (j,Nothing,toPlace) minCost
                        return minCost
                      Just bound -> do
                        if fontJ >= bound then do
                          memorized1 <- opt (j-1) (Just bound) toPlace
                          return memorized1
                        else do
                          memorized1 <- opt (j-1) (Just fontJ) (toPlace - 1)
                          memorized2 <- opt (j-1) (Just bound) toPlace
                          let minCost = customMin costJ memorized1 memorized2
                          modify $ M.insert (j,Nothing,toPlace) minCost
                          return minCost
              customMin :: Int -> Int -> Int -> Int
              customMin v1' v1 v2 =
                case (v1 == -1, v2 == -1) of
                  (True,True) -> -1
                  (False,True) -> v1 + v1'
                  (True,False) -> v2
                  (False,False) -> min (v1 + v1') v2
    in r

main :: IO ()
main = do
  input <- BS.getContents
  let lines = take 3 . C.split '\n' $ input
      Just (n,_) = C.readInt $ lines !! 0
      fonts = readInts $ lines !! 1
      costs = readInts $ lines !! 2
  print $ minCost (A.array (1,n) (zip [1..n] fonts)) (A.array (1,n) (zip [1..n] costs)) n
