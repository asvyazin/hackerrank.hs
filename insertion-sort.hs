import Control.Monad

shifts1 :: Int -> [Int] -> Int
shifts1 n l = length $ filter (< n) l

shifts :: [Int] -> Int
shifts [] = 0
shifts (x:xs) = (shifts1 x xs) + (shifts xs)

getValue :: Read a => IO a
getValue = fmap read $ getLine

getValues :: Read a => IO [a]
getValues = fmap (map read . words) $ getLine

main :: IO ()
main = do
  t <- getValue :: IO Int
  forM_ [1..t] $ \_ -> do
    _ <- getValue :: IO Int
    l <- getValues
    print $ shifts l
    
