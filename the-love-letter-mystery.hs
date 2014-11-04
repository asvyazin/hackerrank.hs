module Main where

import Control.Monad

solve :: String -> Int
solve w = div (myStrLength w (reverse w)) 2
  where myStrLength [] _ = 0
        myStrLength _ [] = 0
        myStrLength (x:xs) (y:ys) = myCharLength x y + myStrLength xs ys
          where myCharLength c1 c2 = abs (fromEnum c1 - fromEnum c2)

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    w <- getLine
    print $ solve w
