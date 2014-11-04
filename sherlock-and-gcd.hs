module Main where

import Control.Applicative
import Control.Monad
import Data.List

solve :: [Int] -> Bool
solve nums = foldl1' gcd nums == 1

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    _ <- getLine
    nums <- (map read . words) <$> getLine
    let result = if solve nums then "YES" else "NO"
    putStrLn result
