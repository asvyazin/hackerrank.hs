module Main where

import Control.Applicative
import Control.Monad
import Data.List

readInts :: IO [Int]
readInts = map read <$> words <$> getLine

solve :: [Int] -> Int -> Int -> Int
solve widths i j = foldl1' min $ take (j - i + 1) $ drop i widths

main :: IO ()
main = do
  [_, t] <- readInts
  widths <- readInts
  replicateM_ t $ do
    [i, j] <- readInts
    print $ solve widths i j
