module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Array


isPalindrome :: Array Int Char -> Int -> Int -> Bool
isPalindrome arr ih il
  | ih >= il = True
  | otherwise =
      let
        h = arr ! ih
        l = arr ! il
      in
        h == l && isPalindrome arr (ih + 1) (il - 1)


toArray :: String -> Array Int Char
toArray s =
  let
    n = length s
  in
    listArray (0, n - 1) s


solve :: String -> Int
solve s =
  let
    arr = toArray s

    (ih0, il0) = bounds arr

    solve' ih il
      | ih >= il = -1
      | otherwise = 
        if (arr ! ih) == (arr ! il)
        then solve' (ih + 1) (il - 1)
        else
          if isPalindrome arr (ih + 1) il
          then ih
          else il
  in
    solve' ih0 il0


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    s <- getLine
    print $ solve s
