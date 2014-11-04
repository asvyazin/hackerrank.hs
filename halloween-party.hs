module Main where

import Control.Monad

solve :: Int -> Int
solve k = let k1 = k `div` 2
              k2 = k - k1
          in k1 * k2

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    k <- readLn
    print $ solve k
