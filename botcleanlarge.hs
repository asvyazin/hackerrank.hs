module Main (main) where

import Control.Monad (mplus)
import Data.List (unfoldr, minimumBy)
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
  | otherwise = do_move_to_point pos $ closestDirtyCell pos board

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

dirtyCells :: Board -> [Point]
dirtyCells = keys . M.filter (== DIRT)

closestDirtyCell :: Point -> Board -> Point
closestDirtyCell p b = minimumBy (\ p1 p2 -> compare (distance p1 p) (distance p2 p)) (dirtyCells b)

distance :: Point -> Point -> Int
distance p1 p2 = (abs $ (x p1) - (x p2)) + (abs $ (y p1) - (y p2))

getList :: Int -> IO[String]
getList n = if n==0 then return [] else do i <- getLine; is <- getList(n-1); return (i:is)

split :: Eq a => a -> [a] -> [[a]]
split sep = takeWhile (not . null) . unfoldr (Just . span (/= sep) . dropWhile (== sep))

type Dim = (Int,Int)
type BotPos = (Int,Int)

nextMove :: BotPos -> Dim -> [String] -> String
nextMove (posY, posX) _ board = show $ do_move (Point {x = posX, y = posY}) (readBoard board)

readBoard :: [String] -> Board
readBoard board = let b = map (zip [0..]) board
                      bb = zip [0..] b
                  in fromList [(Point {x = xx, y = yy}, fromChar c) | (yy, zz) <- bb, (xx, c) <- zz]
                     
fromChar :: Char -> Cell
fromChar 'b' = BOT
fromChar 'd' = DIRT
fromChar 'o' = UNKNOWN
fromChar _ = CLEAR

main :: IO ()
main = do
   b <- getLine
   i <- getLine
   let bot = ((read (head s))::Int,(read (head(tail s))::Int)) where s = split (' ') b
   let dim = ((read (head s))::Int,(read (head(tail s))::Int)) where s = split (' ') i
   grid <- getList (fst dim)
   putStrLn $ nextMove bot dim grid
