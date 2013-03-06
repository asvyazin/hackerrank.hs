import Data.List

beginsWith :: String -> String -> Bool
beginsWith p v = begins1 p v 1
  where
    begins1 _ [] _ = True
    begins1 [] _ _ = False
    begins1 (x:xs) (y:ys) k
      | x == y = begins1 xs ys k
      | k == 0 = False
      | otherwise = begins1 xs ys (k - 1)

findVirusIndices :: String -> String -> [Int]
findVirusIndices p v = findWithCounter p v 0
  where
    findWithCounter [] _ _ = []
    findWithCounter p@(x:xs) v k
      | beginsWith p v = k : findWithCounter xs v (k + 1)
      | otherwise = findWithCounter xs v (k + 1)

str2Int :: String -> Int
str2Int = read

getPair :: IO (String, String)
getPair = do
  s1 <- getLine
  s2 <- getLine
  return (s1, s2)

getPair1 :: IO (String, String)
getPair1 = do
  _ <- getLine
  getPair
  
getPairs :: IO [(String, String)]
getPairs = do
  pairsCount <- fmap str2Int getLine
  sequence $ (getPair : replicate (pairsCount - 1) getPair1)
  
printIndices :: [Int] -> IO ()
printIndices = putStrLn . intercalate " " . map show
  
main :: IO ()
main = do
  pairs <- getPairs
  mapM_ printIndices $ map (uncurry findVirusIndices) pairs