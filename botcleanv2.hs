module Main (main) where

import Control.Monad (mplus)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Map (Map, fromList, (!))

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

data Move = LEFT | RIGHT | UP | DOWN | CLEAN deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)
data Cell = BOT | DIRT | CLEAR | UNKNOWN deriving (Show, Eq)
type Board = Map Point Cell

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
  | (board ! pos) == DIRT = CLEAN
  | otherwise = case closestDirtyCell 5 pos board of
                  Nothing -> move (x pos) (y pos)
                  Just cell -> moveTo cell
                    where moveTo dest = do_move_to_point pos dest

closestDirtyCell :: Int -> Point -> Board -> Maybe Point
closestDirtyCell k p b = find (\q -> (b ! q) == DIRT) $ neighbors k p

neighbors :: Int -> Point -> [Point]
neighbors k Point {x = xx, y = yy} = filter (isValidPoint k) [Point {x = xx, y = yy-1},Point {x = xx+1, y = yy},Point {x = xx+1, y = yy-1},Point {x = xx, y = yy+1},Point {x = xx+1, y = yy+1},Point {x = xx-1, y = yy},Point {x = xx-1, y = yy+1},Point {x = xx-1, y = yy-1}]

isValidPoint :: Int -> Point -> Bool
isValidPoint k Point {x = xx, y = yy} = xx >= 0 && yy >= 0 && xx < k && yy < k

move :: Int -> Int -> Move
move 0 4 = UP
move 0 _ = RIGHT
move _ 0 = DOWN
move 4 _ = LEFT
move _ 4 = UP
move 2 2 = RIGHT
move 1 1 = RIGHT
move 2 1 = RIGHT
move 3 1 = DOWN
move 3 2 = DOWN
move 3 3 = LEFT
move 2 3 = LEFT
move 1 3 = UP
move 1 2 = UP

readPosition :: String -> Point
readPosition pos = let [yy, xx] = map read $ words pos
                   in Point {x = xx, y = yy}

readBoard :: [String] -> Board
readBoard board = let b = map (zip [0..]) board
                      bb = zip [0..] b
                  in fromList [(Point {x = xx, y = yy}, fromChar c) | (yy, zz) <- bb, (xx, c) <- zz]
                     
fromChar :: Char -> Cell
fromChar 'b' = BOT
fromChar 'd' = DIRT
fromChar 'o' = UNKNOWN
fromChar _ = CLEAR

next_move :: String -> [String] -> String
next_move pos board = show $ do_move (readPosition pos) (readBoard board)

main :: IO()
main = do
    pos <- getLine
    board <- getList 5
    putStrLn $ next_move pos board
