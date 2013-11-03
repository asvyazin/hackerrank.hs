module Main (main) where

import Control.Monad (mplus)
import Data.Map (Map, fromList, (!), keys)
import qualified Data.Map as M (filter)
import Data.Maybe (fromJust)

data Move = LEFT | RIGHT | UP | DOWN | CLEAN deriving (Show)
data Point = Point {x :: Int, y :: Int} deriving (Show, Ord, Eq)
data Cell = BOT | DIRT | CLEAR | UNKNOWN deriving (Show, Eq)
type Board = Map Point Cell

do_move :: Point -> Board -> Move
do_move pos board
  | (board ! pos) == DIRT = CLEAN
  | otherwise = let dirtyCell = head $ keys $ M.filter (== DIRT) board
                in do_move_to_point pos dirtyCell

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

getList :: Int -> IO [String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

nextMove :: String -> [String] -> String
nextMove pos board = show $ do_move (readPosition pos) (readBoard board)

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

main :: IO()
main = do
    pos <- getLine
    board <- getList 5
    putStrLn $ nextMove pos board
