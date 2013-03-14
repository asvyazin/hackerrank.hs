plus n m = (n + m) `mod` 10007
mult n m = (n * m) `mod` 10007
sum10007 = foldl plus 0

up = (map up' [0..] !!)
  where up' 0 = 0
        up' 1 = 1
        up' 2 = 1
        up' n = (up0 n) `plus` (up1 n)
        
up0 = (map up0' [0..] !!)
  where up0' 0 = 0
        up0' 1 = 0
        up0' 2 = 1
        up0' 3 = 1
        up0' n = up (n - 1)
        
up1 = (map up1' [0..] !!)
  where up1' 0 = 0
        up1' n = sum10007 $ map (\i -> (up0 i) `mult` (up (n - i + 1))) [2..(n - 1)]

answer 1 = 1
answer n = 2 `mult` (up n)

main = do
  k <- fmap (read :: String -> Int) getLine
  print $ answer k