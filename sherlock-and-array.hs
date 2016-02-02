module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B


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


solve :: TestCase -> Bool
solve tc =
  let
    n = sum tc

    iter [] res _ = res
    iter (x:xs) res currentLeft =
      let
        currentRight = n - x - currentLeft
      in
        iter xs ((currentRight == currentLeft):res) (currentLeft + x)
  in
    or $ iter tc [] 0


main :: IO ()
main = do
  tcs <- parseOnly input <$> B.getContents >>= guardEither
  forM_ tcs $ \tc ->
    putStrLn $ if solve tc then "YES" else "NO"
