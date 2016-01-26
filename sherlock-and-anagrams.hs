module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Prelude hiding (length, tail)


substrings :: [a] -> [[a]]
substrings [] = []
substrings s@(_:xs) =
  inits_ s ++ substrings xs
  where
    inits_ = tail . inits


isAnagramPair :: String -> String -> Bool
isAnagramPair s1 s2 =
  let
    ss1 = sort s1
    ss2 = sort s2
  in
    ss1 == ss2


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) =
  [(x, y) | y <- xs] ++ pairs xs


solve :: String -> Int
solve str =
  let
    g = groupBy ((==) `on` length) $ sortBy (compare `on` length) $ substrings str

    solveG =
      length . filter (uncurry isAnagramPair) . pairs
  in
    sum $ map solveG g 


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str <- getLine
    print $ solve str
