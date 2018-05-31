module Main where


notPalindrome :: String -> Bool
notPalindrome str =
  str /= reverse str

substrings :: String -> [String]
substrings [] = [""]
substrings str@(x:xs) =
  let substrings' = substrings xs
  in concat [[str], substrings']


main = do
  str <- getLine
  let ok = filter notPalindrome $ substrings str
  if null ok then
    putStrLn "0"
  else
    print $ maximum $ map length ok
