module Main where

import Control.Monad
import Data.List
import Data.Set (fromList, intersection, size)

solve :: [String] -> Int
solve = size . foldl1' intersection . map fromList

main :: IO ()
main = do
  t <- readLn :: IO Int
  ws <- replicateM t getLine
  print $ solve ws
