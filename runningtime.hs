import Data.List

insertToList :: [Int] -> Int -> ([Int], Int)
insertToList = insertRecurse [] 0
  where insertRecurse :: [Int] -> Int -> [Int] -> Int -> ([Int], Int)
        insertRecurse p k [] n = (p ++ [n], k)
        insertRecurse p k l@(x:xs) n
          | n <= x = (p ++ [n] ++ l, k)
          | otherwise = insertRecurse (p ++ [x]) (k + 1) xs n
                        
answer :: [Int] -> Int
answer (x:xs) = answerRecurse [x] xs
  where answerRecurse :: [Int] -> [Int] -> Int
        answerRecurse sorted [] = 0
        answerRecurse sorted unsorted@(x:xs) = (answerRecurse newSorted xs) + delta
          where (newSorted, delta) = insertToList sorted x
        
str2Int = read :: String -> Int

main = do
  count <- fmap str2Int getLine
  numbers <- fmap (map str2Int . words) getLine
  print $ answer $ reverse numbers
  