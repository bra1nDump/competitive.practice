import System.Random
import Control.Monad


main :: IO ()
main = do
  gen <- getStdGen
  let n = 20000
  putStrLn $ show n
  let (tests,_) = foldl (\(xs,gen) _ ->
                       let (rand,gen') = randomR ((-4000 :: Int),4000) gen
                       in (rand:xs,gen'))
              ([],gen) [1..n]
  forM_ tests (\test -> do
                  putStrLn "1"
                  putStrLn $ show test)

