module Main where

import Control.Monad
import Data.List (sort)
import qualified Data.Set as S

solveSet :: Int -> Int -> Int -> S.Set Int
solveSet 1 _ _ = S.empty
solveSet 2 a b = S.fromList [a, b]
solveSet n a b = let previous = solveSet (n - 1) a b
                     s1 = S.map (+ a) previous
                     s2 = S.map (+ b) previous
                 in s1 `S.union` s2

solve :: Int -> Int -> Int -> [Int]
solve n a = sort . S.toList . solveSet n a

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn
    a <- readLn
    b <- readLn
    putStrLn $ unwords $ map show $ solve n a b
