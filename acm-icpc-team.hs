module Main where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

zeroCode :: Int
zeroCode = fromEnum '0'

readBitInteger :: String -> Integer
readBitInteger = foldl1' (\ r b -> shiftL r 1 .|. b) . map (\c -> toEnum (fromEnum c - zeroCode))

getBitInteger :: IO Integer
getBitInteger = readBitInteger <$> getLine

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs (x:xs) = map (\y -> (x, y)) xs ++ pairs xs

solve :: [Integer] -> (Int, Int)
solve nums = let bitNums = map (\ (x, y) -> popCount (x .|. y)) $ pairs nums
                 maxBits = maximum bitNums
             in (maxBits, length $ filter (== maxBits) bitNums)

main :: IO ()
main = do
  [n, _] <- getInts
  bits <- replicateM n getBitInteger
  let (res1, res2) = solve bits
  print res1
  print res2
