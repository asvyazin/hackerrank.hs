similarity :: String -> String -> Int
similarity (x:xs) (y:ys)
  | x == y = 1 + (similarity xs ys)
  | otherwise = 0
similarity _ _ = 0

suffixes :: String -> [String]
suffixes [] = []
suffixes s@(x:xs) = s : (suffixes xs)

suffixesSimilarity :: String -> Int
suffixesSimilarity s = sum $ map (similarity s) $ suffixes s

answer :: [String] -> [Int]
answer = map suffixesSimilarity

str2Int = read :: String -> Int

main = do
  linesCount <- fmap str2Int getLine
  ls <- fmap (take linesCount . lines) getContents
  mapM_ print $ answer ls