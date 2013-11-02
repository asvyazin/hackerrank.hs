module Main (main) where

import Control.Monad (mplus)
import Data.List (minimumBy)
import qualified Data.Map as M (Map, fromList, (!), filter, keys)
import Data.Maybe (fromJust)

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

data Move = LEFT | RIGHT | UP | DOWN | CLEAN deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)
data Cell = BOT | DIRT | CLEAR deriving (Show, Eq)
type Board = M.Map Point Cell

fromChar :: Char -> Cell
fromChar 'b' = BOT
fromChar 'd' = DIRT
fromChar _ = CLEAR

isDirty :: Cell -> Bool
isDirty = (== DIRT)

readPosition :: String -> Point
readPosition pos = let [yy, xx] = map read $ words pos
                   in Point {x = xx, y = yy}

readBoard :: [String] -> Board
readBoard board = let b = map (zip [0..]) board
                      bb = zip [0..] b
                  in M.fromList [(Point {x = xx, y = yy}, fromChar c) | (yy, zz) <- bb, (xx, c) <- zz]

dirtyCells :: Board -> [Point]
dirtyCells = M.keys . M.filter isDirty

closestDirtyCell :: Point -> Board -> Point
closestDirtyCell p b = minimumBy (\ p1 p2 -> compare (distance p1 p) (distance p2 p)) (dirtyCells b)

distance :: Point -> Point -> Int
distance p1 p2 = (abs $ (x p1) - (x p2)) + (abs $ (y p1) - (y p2))

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

do_move_to_point :: Point -> Point -> Move
do_move_to_point Point {x = fromX, y = fromY} Point {x = toX, y = toY} = fromJust $ (moveVertical fromY toY) `mplus` (moveHorizontal fromX toX)

do_move :: Point -> Board -> Move
do_move pos board
  | isDirty (board M.! pos) = CLEAN
  | otherwise = do_move_to_point pos $ closestDirtyCell pos board

next_move :: String -> [String] -> String
next_move pos board = show $ do_move (readPosition pos) (readBoard board)

main :: IO()
main = do
    pos <- getLine
    board <- getList 5
    putStrLn $ next_move pos board
