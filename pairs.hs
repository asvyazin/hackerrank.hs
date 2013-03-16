getLineAsList = fmap (map read . words) getLine
getLineOfNumbers = getLineAsList :: IO [Int]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = (map (\y -> (x, y)) xs) ++ (pairs xs)

main = do
  [_, k] <- getLineOfNumbers
  numbers <- getLineOfNumbers
  let isGoodPair (x, y) = (abs (x - y)) == k
      result = length $ filter isGoodPair $ pairs numbers
    in print result