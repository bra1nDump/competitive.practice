import System.Random
import System.IO

main :: IO ()
main = do
  gen <- getStdGen
  let nums = take 20000 $ randomRs ((-100000),100000) gen
      _ = nums :: [Int]
      testStr = show (length nums) ++ "\n"
        ++ (concat . map (\n -> show n ++ " ") $ nums)
  writeFile "test.txt" testStr
