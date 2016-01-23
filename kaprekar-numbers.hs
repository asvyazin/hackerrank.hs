module Main (main) where


import Control.Applicative
import Data.List


digits :: Int -> [Int]
digits n =
  dig n []
  where
    dig p l
      | p < 10 = p : l
      | otherwise =
        let
          (d, m) = divMod p 10
        in
          dig d (m : l)


digitsCount :: Int -> Int
digitsCount =
  length . digits


fromDigits :: [Int] -> Int
fromDigits =
  foldl' (\n d -> 10 * n + d) 0


isKaprekar :: Int -> Bool
isKaprekar n =
  let
    d = digitsCount n 
    sq = n * n
    sqDigits = digits sq
    (ds1, ds2) = splitAt (length sqDigits - d) sqDigits
    d1 = fromDigits ds1
    d2 = fromDigits ds2
  in
    n == d1 + d2


solve :: Int -> Int -> [Int]
solve p q =
  filter isKaprekar [p..q]


main :: IO ()
main = do
  p <- (read :: String -> Int) <$> getLine
  q <- (read :: String -> Int) <$> getLine
  let arr = solve p q
  putStrLn $ if null arr
             then "INVALID RANGE"
             else unwords $ map show arr
