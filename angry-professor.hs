module Main (main) where


import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Scientific as S
import qualified Data.Text.IO as T


guardMaybe :: Monad m => String -> Maybe a -> m a
guardMaybe err m =
  case m of
    Nothing ->
      fail err
    Just res ->
      return res


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


parseInteger :: (Integral a, Bounded a) => Parser a
parseInteger =
  (S.toBoundedInteger <$> scientific) >>= guardMaybe "Expected number"


parseIntegersN :: (Integral a, Bounded a) => Int -> Parser [a]
parseIntegersN 0 = return []
parseIntegersN 1 = do
  i <- parseInteger
  return [i]
parseIntegersN n = do
  i0 <- parseInteger
  is <- count (n - 1) (space *> parseInteger)
  return (i0:is)


testCase :: Parser (Int, [Int])
testCase = do
  n <- parseInteger
  k <- space *> parseInteger
  _ <- endOfLine
  nums <- parseIntegersN n
  return (k, nums)


input :: Parser [(Int, [Int])]
input = do
  t <- parseInteger
  count t (endOfLine *> testCase)


solve :: [(Int, [Int])] -> IO ()
solve =
  mapM_ solve1
  where
    solve1 (k, nums) =
      putStrLn $
      if length (filter (<= 0) nums) < k
      then "YES"
      else "NO"


main :: IO ()
main = do
  tcs <- (parseOnly input <$> T.getContents) >>= guardEither
  solve tcs
