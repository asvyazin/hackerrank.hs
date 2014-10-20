import Control.Monad
import qualified Data.Map as M

getValue :: Read a => IO a
getValue = liftM read $ getLine

getValues :: Read a => IO [a]
getValues = liftM (map read . words) $ getLine

convertAnswer :: Bool -> String
convertAnswer True = "YES"
convertAnswer False = "NO"

insert1 :: M.Map Int Int -> Int -> M.Map Int Int
insert1 m k = M.alter increment k m
  where increment Nothing = Just 1
        increment (Just n) = Just (n + 1)

remove1 :: M.Map Int Int -> Int -> M.Map Int Int
remove1 m k = M.alter decrement k m
  where decrement Nothing = Nothing
        decrement (Just 0) = Nothing
        decrement (Just 1) = Nothing
        decrement (Just n) = Just (n - 1)

sum1 :: M.Map Int Int -> Int
sum1 = M.foldlWithKey (\s k l -> s + (k * l)) 0

solveLists :: [Int] -> [Int] -> Int -> Int -> Bool
solveLists a b n k = solve a (list2Map b) n k

list2Map :: [Int] -> M.Map Int Int
list2Map = foldl insert1 M.empty

solve :: [Int] -> M.Map Int Int -> Int -> Int -> Bool
solve [] _ _ _ = True
solve a@(a0:aTail) b n k
  | sumA + sumB < n * k = False
  | otherwise = any tryElem $ splitMap b
  where tryElem (bCur, bNext) = (a0 + bCur >= k) && (solve aTail bNext (n - 1) k)
        sumA = sum a
        sumB = sum1 b

splitMap :: M.Map Int Int -> [(Int, M.Map Int Int)]
splitMap m = map splitMapAt [0..(mLen - 1)]
  where mLen = M.size m
        splitMapAt i = (mCur, mNext)
          where (mCur, _) = M.elemAt i m
                mNext = remove1 m mCur

main :: IO ()
main = do
  cases <- getValue :: IO Int
  forM_ [1..cases] $ \_ -> do
    [n, k] <- getValues
    a <- getValues
    b <- getValues
    putStrLn $ convertAnswer $ solveLists a b n k
