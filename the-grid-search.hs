module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
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
  UArray (Int, Int) Char


getMatrix :: Int -> Int -> [String] -> Matrix
getMatrix r c rows =
  array ((0, 0), (r - 1, c - 1)) $ do
    (i, row) <- zip [0..] rows
    (j, x) <- zip [0..] row
    return ((i, j), x)


type TestCase = (Matrix, Matrix)


testCase :: Parser TestCase
testCase = do
  (r1, c1, rows1) <- matrixInput
  (r2, c2, rows2) <- endOfLine *> matrixInput
  return (getMatrix r1 c1 rows1, getMatrix r2 c2 rows2)
  where
    matrixInput = do
      r <- parseInteger
      c <- space *> parseInteger
      rows <- count r (endOfLine *> many1 digit)
      return (r, c, rows)


type Input = [TestCase]


input :: Parser Input
input = do
  t <- parseInteger
  count t (endOfLine *> testCase)


solve :: Matrix -> Matrix -> Bool
solve m1 m2 =
  let
    (_, (im2, jm2)) =
      bounds m2

    (_, (im1, jm1)) =
      bounds m1

    searchFrom :: (Int, Int) -> Bool
    searchFrom (i, j) =
      all (\(k, l) -> (m1 ! (i + k, j + l)) == (m2 ! (k, l))) $ indices m2

    indicesToSearch :: [(Int, Int)]
    indicesToSearch =
      [(i, j) | i <- [0..(im1 - im2)], j <- [0..(jm1 - jm2)]]
  in
   any searchFrom indicesToSearch


main :: IO ()
main = do
  res <- parseOnly input <$> T.getContents
  case res of
    Left err ->
      fail err
    Right tc ->
      forM_ tc $ \ (m1, m2) -> do
        let b = solve m1 m2
        putStrLn $ if b then "YES" else "NO"
