module Main (main) where


import Control.Applicative


data Date =
  Date
  { year :: Int
  , month :: Int
  , day :: Int
  } deriving (Eq)


instance Ord Date where
  (Date y1 m1 d1) <= (Date y2 m2 d2) =
    y1 < y2 ||
    (y1 == y2 && (m1 < m2 ||
                  (m1 == m2 && d1 <= d2)))


getDate :: IO Date
getDate = do
  [d, m ,y] <- (map read . words) <$> getLine
  return $ Date y m d


solve :: Date -> Date -> Int
solve actual expected
  | actual <= expected = 0
  | year actual == year expected && month actual == month expected = 15 * (day actual - day expected)
  | year actual == year expected = 500 * (month actual - month expected)
  | otherwise = 10000


main :: IO ()
main = do
  actualDate <- getDate
  expectedDate <- getDate
  print $ solve actualDate expectedDate
