module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Array
import Data.List


solve :: Int -> Int -> [Int] -> Int
solve n k nums =
  let
    v0 = listArray (0, n - 1) $ sort nums

    value begin end = (v0 ! end) - (v0 ! begin)
  in
    minimum [value i (i + k - 1) | i <- [0..(n - k)]]


main :: IO ()
main = do
  let getInt = (read :: String -> Int) <$> getLine
  n <- getInt
  k <- getInt
  nums <- replicateM n getInt
  print $ solve n k nums
