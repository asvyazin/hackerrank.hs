module Main (main) where


import Control.Applicative


fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)


main :: IO ()
main = do
  n <- (read :: String -> Integer) <$> getLine
  print $ fact n
