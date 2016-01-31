module Main (main) where


import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    n <- (read :: String -> Word32) <$> getLine
    let n' = complement n
    print n'
