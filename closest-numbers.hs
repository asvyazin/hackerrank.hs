module Main (main) where


import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Data.List


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
  x <- signed decimal
  xs <- count (n - 1) (space *> signed decimal)
  return (x:xs)


input :: Parser [Int]
input = do
  n <- decimal
  endOfLine *> numbers n


solve :: [Int] -> [Int]
solve nums =
  let
    sortedNums = sortBy (flip compare) nums

    iter [] _ currentRes =
      currentRes
    iter [_] _ currentRes =
      currentRes
    iter (x1:xs@(x2:_)) currentDiff currentRes
      | x1 - x2 < currentDiff = iter xs (x1 - x2) [x2, x1]
      | x1 - x2 == currentDiff = iter xs currentDiff (x2:x1:currentRes)
      | otherwise = iter xs currentDiff currentRes
  in
    iter sortedNums maxBound []


main :: IO ()
main = do
  nums <- parseOnly input <$> B.getContents >>= guardEither
  putStrLn $ unwords $ map show $ solve nums
