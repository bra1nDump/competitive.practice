module Main where

import Data.List

stones :: [(String,String)]
stones = [
    ("green", "Time"),
    ("purple", "Power"),
    ("blue", "Space"),
    ("orange", "Soul"),
    ("red","Reality"),
    ("yellow","Mind")
    ]

main :: IO ()
main = do
  n <- fmap (read) getLine
  colors <- sequence $ Prelude.take n . repeat $ getLine
  let allColors = map fst stones
      remaining = Prelude.filter (\(color,_) -> not . isInfixOf [color] $ colors) stones
  print $ length remaining
  mapM_ (\(_,name) -> putStrLn name) remaining
