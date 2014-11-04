module Main where

import Control.Applicative
import Control.Monad

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

solve :: Int -> Int -> Int -> Int
solve n c m = let chocolate0 = n `div` c
              in loop chocolate0 chocolate0
  where loop chocolate wrappers = let (chocolate1, wrappers1) = divMod wrappers m
                                  in if chocolate1 == 0
                                     then chocolate
                                     else loop (chocolate + chocolate1) (chocolate1 + wrappers1)

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    [n, c, m] <- getInts
    print $ solve n c m
