module Main (main) where


import Control.Applicative
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


parseIntegersN :: (Integral a, Bounded a) => Int -> Parser [a]
parseIntegersN 0 = return []
parseIntegersN 1 = do
  i <- parseInteger
  return [i]
parseIntegersN n = do
  i0 <- parseInteger
  is <- count (n - 1) (space *> parseInteger)
  return (i0:is)


input :: Parser [Int]
input = do
  n <- parseInteger
  _ <- endOfLine
  parseIntegersN n


main :: IO ()
main = do
  result <- parseOnly input <$> T.getContents
  case result of
    Left err ->
      fail err
    Right arr ->
      print $ sum arr
