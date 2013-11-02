module Main where

import Control.Monad (msum, mplus)
import Data.List (findIndex)
import Data.Maybe (isJust, fromJust)

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)
-- Head ends here
                                           
data Move = LEFT | RIGHT | UP | DOWN deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Show)
                                           
findChar :: Char -> [String] -> Point
findChar c ss = let indices = map (findIndex (== c)) ss
                    xx = fromJust $ msum indices
                    yy = fromJust $ findIndex isJust indices
                in Point {x = xx, y = yy}

moveVertical :: Int -> Int -> Maybe Move
moveVertical fromY toY
  | fromY == toY = Nothing
  | fromY < toY = Just DOWN
  | otherwise = Just UP

moveHorizontal :: Int -> Int -> Maybe Move
moveHorizontal fromX toX
  | fromX == toX = Nothing
  | fromX < toX = Just RIGHT
  | otherwise = Just LEFT

doMovePoints :: Point -> Point -> Move
doMovePoints Point {x = fromX, y = fromY} Point {x = toX, y = toY} = fromJust $ (moveVertical fromY toY) `mplus` (moveHorizontal fromX toX)

doMove :: [String] -> Move
doMove grid = let mp = findChar 'm' grid
                  pp = findChar 'p' grid
              in doMovePoints mp pp

nextMove :: Int -> [String] -> String
nextMove _ grid = show $ doMove grid

-- Tail starts here
main :: IO ()
main = do
    n <- getLine
    _ <- getLine
    let i = read n
    grid <- getList i
    putStrLn $ nextMove i grid
