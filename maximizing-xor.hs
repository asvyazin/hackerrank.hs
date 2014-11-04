module Main where

import Data.Bits

maxIndexOfOne :: Int -> Int
maxIndexOfOne n = maximum $ filter (testBit n) [0..(bitSize n)]
  
maxXor :: Int -> Int -> Int
maxXor l r = let m = l `xor` r
                 idx = maxIndexOfOne m
             in bit (idx + 1) - 1

main :: IO ()
main = do
  l <- readLn
  r <- readLn
  print $ maxXor l r
