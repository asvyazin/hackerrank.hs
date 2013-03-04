commonPrefixLength :: Int -> String -> String -> Int
commonPrefixLength _ [] _ = 0
commonPrefixLength _ _ [] = 0
commonPrefixLength k (x:xs) (y:ys)
  | x == y = 1 + (commonPrefixLength k xs ys)
  | k > 0 = 1 + (commonPrefixLength (k - 1) xs ys)
  | otherwise = 0

suffixes :: String -> [String]
suffixes [] = []
suffixes s@(x:xs) = s : (suffixes xs)

cartesianProduct1 :: a -> [b] -> [(a, b)]
cartesianProduct1 x = map (\y -> (x, y))

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct [] _ = []
cartesianProduct (x:xs) l = (cartesianProduct1 x l) ++ (cartesianProduct xs l)

commonSubstring :: Int -> String -> String -> Int
commonSubstring k s1 s2 =
  let
    prefixLength = commonPrefixLength k
    lengths = map (uncurry prefixLength) $ cartesianProduct (suffixes s1) (suffixes s2)
  in
   maximum lengths
   
str2Int :: String -> Int
str2Int = read
   
answer :: String -> Int
answer line =
  let
    [ks, s1, s2] = words line
  in
    commonSubstring (str2Int ks) s1 s2
   
main :: IO ()
main = do
  linesCount <- fmap str2Int getLine
  ls <- fmap (take linesCount . lines) getContents
  mapM_ print $ map answer ls