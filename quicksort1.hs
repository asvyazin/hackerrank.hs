import Data.List

qsPartition :: [Int] -> [Int]
qsPartition (x:xs) = [t | t <- xs, t < x] ++ [x] ++ [t | t <- xs, t >= x]

str2Int = read :: String -> Int

main = do
  count <- fmap str2Int getLine
  numbers <- fmap (map str2Int . words) getLine
  putStrLn $ intercalate " " $ map show $ qsPartition numbers