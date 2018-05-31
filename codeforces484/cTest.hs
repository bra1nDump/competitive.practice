import System.Environment

main :: IO ()
main = do
  verticeCount <- fmap read . fmap head $ getArgs
  print verticeCount
  mapM_ (\(v1,v2) -> putStrLn $ show v1 ++ " " ++ show v2) [(v,v + 1) | v <- [1..verticeCount - 1]] 
