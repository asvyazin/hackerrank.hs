module Main (main) where


import Control.Applicative
import qualified Data.List as L
import Data.Word


solve :: Int -> Int -> [Word] -> Word
solve n k prices =
  let
    coeff = take n $ L.concatMap (replicate k) [1..]
    sortedPrices = L.sortBy (flip compare) prices
  in
    sum $ L.zipWith (*) coeff sortedPrices


main :: IO ()
main = do
  [n, k] <- (map read . words) <$> getLine
  prices <- (map read . words) <$> getLine
  print $ solve n k prices
