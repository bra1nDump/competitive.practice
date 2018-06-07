module Main (main) where

import Data.List

isMaximum :: [Char] -> Bool
isMaximum xs@(x:y:ys) =
  let twoLeadingZeros = x == '0' && y == '0'
      [tail', tail''] = drop (length ys) xs
      twoTailingZeros = tail' == '0' && tail'' == '0'
      trippleZeros = elem ('0','0','0') $ zip3 xs (y:ys) ys
      containsTwoOnes = elem ('1','1') $ zip xs (y:ys)
  in not $ twoLeadingZeros || twoTailingZeros || trippleZeros || containsTwoOnes
isMaximum [c] = c == '1'
isMaximum _ = True

main :: IO ()
main = do
  _ <- getLine
  chairs <- getLine
  putStr (if isMaximum chairs then "Yes" else "No")    
