module Main (main) where


import Control.Applicative
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8
import Prelude hiding (take)
import Data.Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import Data.Word


type MyVector =
  V.Vector String


{-# INLINE myPopCount #-}
myPopCount :: String -> String -> Int
myPopCount b1 b2 =
  L.foldr (\(bb1, bb2) c -> if bb1 == '1' || bb2 == '1' then c + 1 else c) 0 $ zip b1 b2


guardEither :: Monad m => Either String a -> m a
guardEither e =
  case e of
    Left err ->
      fail err
    Right res ->
      return res


parseBitInteger :: Int -> Parser String
parseBitInteger k =
  B.unpack <$> take k


input :: Parser MyVector
input = do
  n <- decimal
  k <- space *> decimal
  V.generateM n $ \_ -> endOfLine *> parseBitInteger k


zeroCode :: Int
zeroCode = fromEnum '0'


solve :: MyVector -> (Int, Int)
solve nums =
  let
    n = V.length nums

    pairs = do
      i <- [0..(n - 2)]
      j <- [(i + 1)..(n - 1)]
      return (i, j)

    iter [] curMax curCount =
      (curMax, curCount)
    iter ((i, j):xs) curMax curCount =
      let
        x = nums V.! i
        y = nums V.! j
        newMax = myPopCount x y
      in
        if newMax > curMax
        then iter xs newMax 1
        else
          if newMax == curMax
          then iter xs curMax (curCount + 1)
          else iter xs curMax curCount
  in
    iter pairs minBound 0


main :: IO ()
main = do
  bits <- (parseOnly input <$> B.getContents) >>= guardEither
  let (res1, res2) = solve bits
  print res1
  print res2
