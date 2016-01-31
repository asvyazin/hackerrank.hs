module Main (main) where


import Control.Applicative
import Control.Monad
import Data.List


maxSubarray :: [Int] -> Int
maxSubarray [] = undefined
maxSubarray (x:xs) =
  fst $ foldl' iter (x, x) xs
  where
    iter (maxSoFar, maxEndingHere) y =
      let
        newMEH = max y (maxEndingHere + y)
        newMSF = max maxSoFar newMEH
      in
        (newMSF, newMEH)


maxSubarray2 :: [Int] -> Int
maxSubarray2 l =
  let
    m = maximum l
  in
    if m <= 0
    then m
    else sum $ filter (> 0) l


getInts :: IO [Int]
getInts =
  (map read . words) <$> getLine


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    _ <- getLine
    nums <- getInts
    let
      s1 = maxSubarray nums
      s2 = maxSubarray2 nums
    putStrLn $ show s1 ++ " " ++ show s2
