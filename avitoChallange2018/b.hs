module Main where

import qualified Data.IntMap.Strict as M
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS

getElements :: IO ((M.IntMap Int,M.IntMap Int))
getElements = do
  input <- fmap (C.split '\n') $ BS.getContents
  let count1 = read . C.unpack . head $ input
      list1 = [(v1, v2) | edgeBS <- take count1 . tail $ input,
               let [v1', v2'] = C.split ' ' edgeBS
                   Just (v1,_) = C.readInt v1'
                   Just (v2,_) = C.readInt v2']
      list2 = [(v1, v2) | edgeBS <- drop (count1 + 2) input, not $ BS.null edgeBS,
               let [v1', v2'] = C.split ' ' edgeBS
                   Just (v1,_) = C.readInt v1'
                   Just (v2,_) = C.readInt v2']
  return (M.fromList list1, M.fromList list2)
      

main = do
  (top, forces) <- getElements
  let income = M.foldl (\acc income ->
                          acc + toInteger income) (0 :: Integer) $ M.unionWith max forces top
  print income 
