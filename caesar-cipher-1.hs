module Main (main) where


import Control.Applicative
import Data.Char


caesarChar :: Int -> Char -> Char
caesarChar k c
  | not $ isAlpha c = c
  | otherwise =
    let
      up = isUpper c
      m = fromEnum 'z' - fromEnum 'a' + 1
      a = fromEnum $ if up then 'A' else 'a'
    in
     toEnum $ ((fromEnum c - a + k) `mod` m) + a


main :: IO ()
main = do
  _ <- getLine
  str <- getLine
  k <- (read :: String -> Int) <$> getLine
  putStrLn $ map (caesarChar k) str
