import Data.List

answer :: Int -> [Int] -> [[Int]]
answer n l = buildRecurse n (reverse l) []
  where buildRecurse :: Int -> [Int] -> [Int] -> [[Int]]
        buildRecurse n [] ll = [(n : ll)]
        buildRecurse n l@(x:xs) ll
          | n >= x = [(reverse l) ++ [n] ++ ll]
          | otherwise = ((reverse l) ++ [x] ++ ll) : (buildRecurse n xs (x : ll))
                        
outputLine :: [Int] -> String
outputLine = intercalate " " . map show

str2Int = read :: String -> Int

main = do
  count <- fmap str2Int getLine
  numbers <- fmap (map str2Int . words) getLine
  let n = last numbers
      l = take (count - 1) numbers
      result = answer n l
    in mapM_ putStrLn $ map outputLine result