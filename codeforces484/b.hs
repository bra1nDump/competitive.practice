module Main (main) where

import Data.List (maximumBy, minimumBy, sortBy)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy (fromList)

getIntegers :: IO [Int]
getIntegers = fmap (map read . words) getLine

-- seatOccupation :: [Bool] -> [(Int, (Int, Int))] -> [Int]
-- seatOccupation [] _ = []
-- seatOccupation (isExtravert:xs) currentSeating =
--   let (n, (weight, taken)) = selectWeight . selectOccupancy $ currentSeating
--         where selectWeight = (if isExtravert then maximumBy else minimumBy)
--                 (\(_, (w, _)) (_, (w', _)) -> compare w w')
--               selectOccupancy = filter (\(_, (_, taken)) ->
--                                           (isExtravert && taken == 1)
--                                           || ((not isExtravert) && taken == 0))
--       newSeating toList . insert n (weight, taken + 1) . fromList $ currentSeating
--   in n : seatOccupation xs newSeating

-- main' :: IO ()
-- main' = do
--   _ <- getLine
--   seatWidths <- getIntegers
--   passengers <- fmap (fmap (== '1')) $ getLine
--   let rows = fmap  (\(n, w) -> (n, (w, 0)))
--         . sortBy (\(_, w) (_, w') -> compare w w')
--         . zip [1..] $ seatWidths
--   mapM_ (\n -> putStr $ show n ++ " ") $ seatOccupation passengers rows


selectSeats :: [Bool] -> Int -> [Int] -> [Int]
selectSeats [] _ _ = []
selectSeats (isExtravert:nextPassengers) nextFreeRow introvertRows =
  if isExtravert then
    let (row:newIntrovertRows) = introvertRows
    in row : selectSeats nextPassengers nextFreeRow newIntrovertRows
  else
    nextFreeRow : selectSeats nextPassengers (nextFreeRow + 1) (nextFreeRow : introvertRows)

main :: IO ()
main = do
  _ <- getLine
  seatWidths <- getIntegers
  passengers <- fmap (fmap (== '1')) getLine
  let rows = map (\(n, _) -> n)
        . sortBy (\(_, w) (_, w') -> compare w w')
        . zip [1..] $ seatWidths
      weightedOrder = selectSeats passengers 1 []
      weightedToOriginal = zip [1..] rows 
      restoredOrder = map (\nW ->
                             let (Just n) = M.lookup nW $ M.fromList weightedToOriginal
                             in n) weightedOrder
    in mapM_ (\n -> putStr $ (show n) ++ " ") restoredOrder
 
