module Main where

main :: IO ()
main = do
  [x,y] <- fmap (map read . words) getLine :: IO [Double]
  let logXY = logBase x y
      yDivX = y / x
  if x == 1 && y == 1
    then putStrLn "="
    else do case compare logXY yDivX of
              LT -> putStrLn ">"
              EQ -> putStrLn "="
              GT -> putStrLn "<"
