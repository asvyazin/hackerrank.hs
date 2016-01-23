module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as B
import Data.Int


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


input :: Parser [(Int64, Int64, Int64, Int64, Int64)]
input = do
  t <- decimal
  count t tc
  where
    tc = do
      b <- endOfLine *> decimal
      w <- space *> decimal
      x <- endOfLine *> decimal
      y <- space *> decimal
      z <- space *> decimal
      return (b, w, x, y, z)


solve :: Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64
solve b w x y z
  | x + z < y = (b * x) + (w * (x + z))
  | y + z < x = (b * (y + z)) + (w * y)
  | otherwise = (b * x) + (w * y)


main :: IO ()
main = do
  tcs <- parseOnly input <$> B.getContents >>= guardEither
  forM_ tcs $ \(b, w, x, y, z) ->
    print $ solve b w x y z
