import Data.List

myQsort :: [Int] -> ([Int], [[Int]])
myQsort [] = ([], [])
myQsort (x:xs) = (newResult, pLeft ++ pRight ++ [newResult])
  where (rLeft, pLeft) = myQsort [t | t <- xs, t < x]
        (rRight, pRight) = myQsort [t | t <- xs, t >= x]
        newResult = rLeft ++ [x] ++ rRight

str2Int = read :: String -> Int

outputLine :: [Int] -> String
outputLine = intercalate " " . map show

main = do
  count <- fmap str2Int getLine
  numbers <- fmap (map str2Int . words) getLine
  let (_, pathToResult) = myQsort numbers
      pathFiltered = filter (\x -> (length x) > 1) pathToResult
    in mapM_ putStrLn $ map outputLine pathFiltered