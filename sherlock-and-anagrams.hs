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


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) =
  [(x, y) | y <- xs] ++ pairs xs


solve :: String -> Int
solve str =
  let
    g = groupBy ((==) `on` length) $ sortBy (compare `on` length) $ map sort $ substrings str

    solveG =
      length . filter (uncurry (==)) . pairs
  in
    sum $ map solveG g 


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str <- getLine
    print $ solve str
