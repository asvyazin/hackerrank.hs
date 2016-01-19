module Main (main) where


import Control.Applicative
import Control.Monad
import Data.List (group)


solve :: String -> Int
solve s =
  length s - length (group s)


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str <- getLine
    print $ solve str
