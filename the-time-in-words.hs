module Main (main) where


import Control.Applicative


num :: Int -> String
num 1 = "one"
num 2 = "two"
num 3 = "three"
num 4 = "four"
num 5 = "five"
num 6 = "six"
num 7 = "seven"
num 8 = "eight"
num 9 = "nine"
num 10 = "ten"
num 11 = "eleven"
num 12 = "twelve"
num 13 = "thirteen"
num 14 = "fourteen"
num 15 = "fifteen"
num 16 = "sixteen"
num 17 = "seventeen"
num 18 = "eighteen"
num 19 = "nineteen"
num 20 = "twenty"
num 30 = "thirty"
num 40 = "forty"
num 50 = "fifty"
num 60 = "sixty"
num 70 = "seventy"
num 80 = "eighty"
num 90 = "ninety"
num n
  | n >= 100 = error "Not supported"
  | otherwise =
    let
      (d, m) = divMod n 10
    in
      num (d * 10) ++ " " ++ num m


solve :: Int -> Int -> String
solve h m
  | m == 0 = num h ++ " o' clock"
  | m == 1 = "one minute past " ++ num h
  | m == 15 = "quarter past " ++ num h
  | m < 30 = num m ++ " minutes past " ++ num h
  | m == 30 = "half past " ++ num h
  | m == 45 = "quarter to " ++ num (h + 1)
  | m == 59 = "one minute to " ++ num (h + 1)
  | otherwise = num (60 - m) ++ " minutes to " ++ num (h + 1)


main :: IO ()
main = do
  h <- (read :: String -> Int) <$> getLine
  m <- (read :: String -> Int) <$> getLine
  putStrLn $ solve h m
