module Main (main) where


import Control.Applicative
import Data.Array
import Data.Attoparsec.Text
import qualified Data.Scientific as S
import qualified Data.Text.IO as T


parseInteger :: (Integral a, Bounded a) => Parser a
parseInteger = do
  mResult <- S.toBoundedInteger <$> scientific
  case mResult of
    Nothing ->
      fail "Expected number"
    Just result ->
      return result


type Matrix =
  Array (Int, Int) Char


showMatrix :: Matrix -> String
showMatrix m =
  let
    (_, (n, _)) = bounds m
    
    row i =
      [m ! (i, j) | j <- [0..n]]
  in
   unlines [row i | i <- [0..n]]


getMatrix :: Int -> [String] -> Matrix
getMatrix n rows =
  array ((0, 0), (n - 1, n - 1)) $ do
    (i, row) <- zip [0..] rows
    (j, x) <- zip [0..] row
    return ((i, j), x)


input :: Parser Matrix
input = do
  n <- parseInteger
  rows <- count n (endOfLine *> many1 digit)
  return $ getMatrix n rows


solve :: Matrix -> Matrix
solve m =
  let
    b@(_, (n, _)) = bounds m

    isCavity (i, j) =
      let
        v = m ! (i, j)
        vUp = m ! (i - 1, j)
        vDown = m ! (i + 1, j)
        vLeft = m ! (i, j - 1)
        vRight = m ! (i, j + 1)
      in 
       i /= 0 && i /= n && j /= 0 && j /= n && v > vUp && v > vDown && v > vLeft && v > vRight
  in
   array b [if isCavity i then (i, 'X') else (i, m ! i)| i <- indices m]


main :: IO ()
main = do
  res <- parseOnly input <$> T.getContents
  case res of
    Left err ->
      fail err
    Right m ->
      putStrLn $ showMatrix $ solve m
