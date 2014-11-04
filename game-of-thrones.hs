module Main where

import Data.List

hasPalindromePermutationOdd :: String -> Bool
hasPalindromePermutationOdd [_] = True
hasPalindromePermutationOdd (x:y:xs) | x == y = hasPalindromePermutationOdd xs
                                     | otherwise = hasPalindromePermutationEven (y:xs)

hasPalindromePermutationEven :: String -> Bool
hasPalindromePermutationEven [] = True
hasPalindromePermutationEven (x:y:xs) | x == y = hasPalindromePermutationEven xs
                                      | otherwise = False

hasPalindromePermutation :: String -> Bool
hasPalindromePermutation word = let len = length word
                                    sortedWord = sort word
                                in if odd len
                                   then hasPalindromePermutationOdd sortedWord
                                   else hasPalindromePermutationEven sortedWord

main :: IO ()
main = do
  word <- getLine
  let result = if hasPalindromePermutation word then "YES" else "NO"
  putStrLn result
