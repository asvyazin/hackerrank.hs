module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Array
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe


type Matrix =
  Array (Int, Int) Int


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


numbers :: Int -> Parser [Int]
numbers 0 =
  return []
numbers 1 = do
  k <- decimal
  return [k]
numbers n = do
  k <- decimal
  ks <- count (n - 1) (space *> decimal)
  return (k:ks)


input :: Parser (Matrix, Int)
input = do
  m <- decimal
  n <- space *> decimal
  r <- space *> decimal
  rows <- count m (endOfLine *> numbers n)
  let arr = array ((0, 0), (m - 1, n - 1)) $ do
        (i, row) <- zip [0..] rows
        (j, el) <- zip [0..] row
        return ((i, j), el)
  return (arr, r)


printMatrix :: Matrix -> IO ()
printMatrix arr =
  let
    (_, (m, n)) = bounds arr

    printRow' i = do
      forM_ [0..(n - 1)] $ \j -> do { putStr $ show $ arr ! (i, j); putChar ' ' }
      putStr $ show $ arr ! (i, n)

    printRow i =
      printRow' i >> putChar '\n'
  in do
    forM_ [0..(m - 1)] printRow
    printRow' m


rotateList :: [a] -> Int -> [a]
rotateList l r =
  let
    (h, t) = splitAt r l
  in
    t ++ h


pathRotateTransformation :: [(Int, Int)] -> Int -> (Int, Int) -> (Int, Int)
pathRotateTransformation [] _ x = x
pathRotateTransformation [_] _ x = x
pathRotateTransformation path r' x =
  let
    n = length path
    r = r' `mod` n
    m = M.fromList $ zip path $ rotateList path r
  in
    fromMaybe x $ M.lookup x m


path0_ :: Int -> Int -> [(Int, Int)]
path0_ m n =
  concat [ [(i, 0) | i <- [0..(m - 1)]]
         , [(m, j) | j <- [0..(n - 1)]]
         , [(i, n) | i <- [m, (m - 1)..1]]
         , [(0, j) | j <- [n, (n - 1)..1]]
         ]


path0 :: Int -> Int -> [(Int, Int)]
path0 m n =
  concat [ [(0, j) | j <- [0..(n - 1)]]
         , [(i, n) | i <- [0..(m - 1)]]
         , [(m, j) | j <- [n, (n - 1)..1]]
         , [(i, 0) | i <- [m, (m - 1)..1]]
         ]


paths :: Int -> Int -> [[(Int, Int)]]
paths m n =
  let
    mn = min (m + 1) (n + 1) `div` 2

    path' x =
      map (\(i, j) -> (i + x, j + x)) $ path0 (m - 2 * x) (n - 2 * x)
  in
    [path' x | x <- [0..(mn - 1)]]


matrixTransformation :: Int -> Int -> Int -> (Int, Int) -> (Int, Int)
matrixTransformation m n r =
  let
    ps = paths m n
    ts = map (`pathRotateTransformation` r) ps
  in
    foldl1' (.) ts


rotateMatrix :: Matrix -> Int -> Matrix
rotateMatrix arr r =
  let
    (_, (m, n)) = bounds arr
  in
    ixmap ((0, 0), (m, n)) (matrixTransformation m n r) arr


main :: IO ()
main = do
  (arr, r) <- parseOnly input <$> B.getContents >>= guardEither
  printMatrix $ rotateMatrix arr r
