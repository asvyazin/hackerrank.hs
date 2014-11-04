module Main where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict

nums2idx :: [Int] -> M.Map Int Int
nums2idx arr = M.fromList $ zip arr [0..]

pairs2nums :: [(Int, Int)] -> [Int]
pairs2nums = nub . sort . loop []
  where loop res [] = res
        loop res ((x, y) : xs) = loop (x : y : res) xs

recalculateWidths :: [Int] -> [Int] -> [Int]
recalculateWidths nums widths = let (n1:ns) = nums
                                in loop [] n1 ns (drop n1 widths)
                                   where loop res _ [] _ = reverse res
                                         loop res segStart (m:ms) ws = let n1 = m - segStart
                                                                           r = minimum $ take (n1 + 1) ws
                                                                       in loop (r:res) m ms $ drop n1 ws

solve__ :: (Int, Int) -> State (M.Map (Int, Int) Int) Int
solve__ (i, j) = do
  cache <- get
  case M.lookup (i, j) cache of
    Just res -> return res
    Nothing -> do
      s1 <- solve__ (i + 1, j)
      s2 <- solve__ (i, j - 1)
      let res = min s1 s2
      modify $ M.insert (i, j) res
      return res

solve2 :: [Int] -> [(Int, Int)] -> [Int]
solve2 widths pairs = let s0 = initWidthsMap widths
                      in evalState (mapM solve__ pairs) s0

solve1 :: [Int] -> [(Int, Int)] -> [Int]
solve1 widths pairs = let nums = pairs2nums pairs
                          idxMap = nums2idx nums
                          newWidths = recalculateWidths nums widths
                      in solve2 newWidths $ map (\ (x, y) -> (idxMap M.! x, (idxMap M.! y) - 1)) pairs

initWidthsMap :: [Int] -> M.Map (Int, Int) Int
initWidthsMap = M.fromList . zipWith (\i x -> ((i, i), x)) [0..]

readInts :: IO [Int]
readInts = map read <$> words <$> getLine

readPairs :: Int -> IO [(Int, Int)]
readPairs t = replicateM t $ do
  [x, y] <- readInts
  return (x, y)

main :: IO ()
main = do
  [_, t] <- readInts
  widths <- readInts
  pairs <- readPairs t
  let results = solve1 widths pairs
  mapM_ print results
