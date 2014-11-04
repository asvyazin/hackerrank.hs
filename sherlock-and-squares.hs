module Main where

import Control.Applicative
import Control.Monad

squares :: [Int]
squares = map square [1..]
  where square x = x * x

solve :: Int -> Int -> Int
solve a b = length $ takeWhile (<= b) $ filter (>= a) squares

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

main :: IO ()
main = do
  t <- readLn
  replicateM_ t $ do
    [a, b] <- getInts
    print $ solve a b
