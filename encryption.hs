module Main (main) where


import Data.Array


getRowsColumn :: Int -> (Int, Int)
getRowsColumn n =
  iter 0 n
  where
    iter begin end
      | begin == end - 1 = finalIter begin end
      | otherwise =
        iter' begin end $ (begin + end) `div` 2

    finalIter begin end
      | begin * begin >= n = (begin, begin)
      | begin * end >= n = (begin, end)
      | otherwise = (end, end)

    iter' begin end middle
      | middle * middle < n = iter middle end
      | otherwise = iter begin middle


solve :: String -> String
solve str =
  let
    n = length str
    
    (r, c) = getRowsColumn n

    emptyCellsCount = r * c - n
    
    arr = listArray ((0, 0), (r - 1, c - 1)) str
    
    strs = [map (\i -> arr ! (i, j1)) [0..(r - 1)] | j1 <- [0..(c - 1 - emptyCellsCount)]] ++ [map (\i -> arr ! (i, j2)) [0..(r - 2)] | j2 <- [(c - emptyCellsCount)..(c - 1)]]
  in
    unwords strs


main :: IO ()
main = do
  str <- getLine
  putStrLn $ solve str
