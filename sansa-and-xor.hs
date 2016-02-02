module Main (main) where


import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.Bits
import Data.List (foldl1')


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


numbers :: Int -> Parser [Int]
numbers 0 = return []
numbers n = do
  x <- decimal
  xs <- count (n - 1) (space *> decimal)
  return (x:xs)


type TestCase = [Int]


type Input = [TestCase]


input :: Parser Input
input = do
  t <- decimal
  let testCase = do
        n <- decimal
        endOfLine *> numbers n
  count t (endOfLine *> testCase)


solve :: TestCase -> Int
solve nums =
  let
    n = length nums
  in
    foldl1' xor $ zipWith (\i x -> if even i then 0 else x) [(i + 1) * (n - i) | i <- [0..(n - 1)]] nums


main :: IO ()
main = do
  tcs <- parseOnly input <$> B.getContents >>= guardEither
  mapM_ (print . solve) tcs
