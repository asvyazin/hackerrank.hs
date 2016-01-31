module Main (main) where


import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Maybe
import qualified Data.Map.Strict as M


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


type TestCase = (Int, [Int])


type Input = [TestCase]


type Solution = (Int, Int)


parseInts :: Int -> Parser [Int]
parseInts 0 = return []
parseInts n = do
  x <- decimal
  xs <- count (n - 1) (space *> decimal)
  return (x:xs)


testCase :: Parser TestCase
testCase = do
  m <- decimal
  n <- endOfLine *> decimal
  nums <- endOfLine *> parseInts n
  return (m, nums)


input :: Parser Input
input = do
  t <- decimal
  count t (endOfLine *> testCase)


solve :: TestCase -> Solution
solve (m, nums) =
  let
    numsSet =
      L.foldl' (\s (x, i) -> M.insertWith (++) x [i] s) M.empty $ zip nums [1..]
      
    possibleResults = filter (\x -> M.member (m - x) numsSet) nums

    tryGetSolution result =
      let
        result' =
          m - result
        i1 =
          head (numsSet M.! result)
        i2 =
          last (numsSet M.! result')
      in
        if i1 == i2
        then Nothing
        else Just (min i1 i2, max i1 i2)
  in
    head $ mapMaybe tryGetSolution possibleResults


showSolution :: Solution -> String
showSolution (n1, n2) =
  show n1 ++ " " ++ show n2


main :: IO ()
main = do
  tcs <- parseOnly input <$> B.getContents >>= guardEither
  mapM_ (putStrLn . showSolution . solve) tcs
