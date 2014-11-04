module Main where

import Control.Applicative
import Data.List

solve :: [Int] -> [Int]
solve = solveSorted . sort
  where solveSorted [] = []
        solveSorted numbers@(x:xs) = let l = length numbers
                                         newNumbers = dropEqual x xs
                                     in (l : solveSorted newNumbers)
          where dropEqual _ [] = []
                dropEqual y ls@(z:zs) | z == y = dropEqual y zs
                                      | otherwise = ls

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

main :: IO ()
main = do
  _ <- getLine
  numbers <- getInts
  mapM_ print $ solve numbers
  
