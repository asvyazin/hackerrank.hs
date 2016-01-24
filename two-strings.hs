module Main (main) where


import Control.Applicative
import Control.Monad
import qualified Data.Set as S


solve :: String -> String -> Bool
solve str1 str2 =
  not $ S.null $ S.intersection (S.fromList str1) (S.fromList str2)


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str1 <- getLine
    str2 <- getLine
    putStrLn $ if solve str1 str2
               then "YES"
               else "NO"
