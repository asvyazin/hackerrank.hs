module Main where

import Control.Monad

normalizePair :: (Int, Int) -> (Int, Int)
normalizePair (x, y) | y >= 0 = (x, y)
                     | otherwise = normalizePair (x - 5, y + 3)

solve :: Int -> Maybe (Int, Int)
solve 1 = Nothing
solve 2 = Nothing
solve 4 = Nothing
solve 7 = Nothing
solve n = Just $ normalizePair (2 * n, -n)

main :: IO ()
main = do
  t <- readLn :: IO Int
  replicateM_ t $ do
    n <- readLn
    case solve n of
      Nothing -> putStrLn "-1"
      Just (x, y) -> do
        replicateM_ (3*x) $ putChar '5'
        replicateM_ (5*y) $ putChar '3'
        putChar '\n'
