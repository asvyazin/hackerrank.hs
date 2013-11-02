module Main where

import Control.Monad (msum)
import Data.List (intercalate, findIndex)
import Data.Maybe (isJust, fromJust)

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here

data Move = LEFT | RIGHT | UP | DOWN deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Show)

pathVertical :: Int -> Int -> [Move]
pathVertical fromY toY
  | fromY == toY = []
  | fromY < toY = replicate (toY - fromY) DOWN
  | otherwise = replicate (fromY - toY) UP

pathHorizontal :: Int -> Int -> [Move]
pathHorizontal fromX toX
  | fromX == toX = []
  | fromX < toX = replicate (toX - fromX) RIGHT
  | otherwise = replicate (fromX - toX) LEFT

pathBetweenPoints :: Point -> Point -> [Move]
pathBetweenPoints Point {x = fromX, y = fromY} Point {x = toX, y = toY} = (pathVertical fromY toY) ++ (pathHorizontal fromX toX)

findChar :: Char -> [String] -> Point
findChar c ss = let indices = map (findIndex (== c)) ss
                    xx = fromJust $ msum indices
                    yy = fromJust $ findIndex isJust indices
                in Point {x = xx, y = yy}

pathToPrincess :: [String] -> [Move]
pathToPrincess grid = let mp = findChar 'm' grid
                          pp = findChar 'p' grid
                      in pathBetweenPoints mp pp
                                           
displayPathtoPrincess :: Int -> [String] -> String
displayPathtoPrincess _ grid = intercalate "\n" $ map show $ pathToPrincess grid

-- Tail starts here
main :: IO ()
main = do
    n <- getLine
    let i = read n
    grid <- getList i
    putStrLn $ displayPathtoPrincess i grid
