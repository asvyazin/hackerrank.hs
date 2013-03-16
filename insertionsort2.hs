import Data.List

insertToList :: [Int] -> Int -> [Int]
insertToList = insertRecurse []
  where insertRecurse :: [Int] -> [Int] -> Int -> [Int]
        insertRecurse p [] n = p ++ [n]
        insertRecurse p l@(x:xs) n
          | n <= x = p ++ [n] ++ l
          | otherwise = insertRecurse (p ++ [x]) xs n
                        
answer :: [Int] -> [[Int]]
answer (x:xs) = answerRecurse [x] xs
  where answerRecurse :: [Int] -> [Int] -> [[Int]]
        answerRecurse sorted [] = []
        answerRecurse sorted unsorted@(x:xs) = (newSorted ++ xs) : (answerRecurse newSorted xs)
          where newSorted = insertToList sorted x
        
outputLine :: [Int] -> String
outputLine = intercalate " " . map show
        
str2Int = read :: String -> Int

main = do
  count <- fmap str2Int getLine
  numbers <- fmap (map str2Int . words) getLine
  mapM_ putStrLn $ map outputLine $ answer numbers
  