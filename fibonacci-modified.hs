module Main (main) where


import Control.Applicative


getInts :: IO [Int]
getInts =
  (map read . words) <$> getLine


solve :: Integer -> Integer -> Int -> Integer
solve a b n =
  solves !! (n - 1)
  where
    solves = a : b : next solves

    next (x : t@(y:_)) = (x + y * y) : next t
    next _ = undefined


main :: IO ()
main = do
  [a, b, n] <- getInts
  print $ solve (fromIntegral a) (fromIntegral b) n
