module Main where

import Control.Applicative
import Control.Monad
import Data.List

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

solve :: Int -> [(Int, Int, Int)] -> Int
solve n tasks = let ss = foldl' (\ s (a, b, k) -> s + (k * (abs (a - b) + 1))) 0 tasks
                in ss `div` n

main :: IO ()
main = do
  [n, m] <- getInts
  tasks <- replicateM m $ do
    [a, b, k] <- getInts
    return (a, b, k)
  print $ solve n tasks
