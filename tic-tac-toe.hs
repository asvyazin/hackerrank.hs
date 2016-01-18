module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord

maxValue :: Ord v =>  s -> (s -> Bool -> [(a, s)]) -> (s -> Bool) -> (s -> v) -> (Maybe a, v)
maxValue state moves term eval
  | term state = (Nothing, eval state)
  | otherwise = maximumBy (comparing snd) $ map util $ moves state True
  where util (action, state') = (Just action, snd $ minValue state' moves term eval)

minValue :: Ord v => s -> (s -> Bool -> [(a, s)]) -> (s -> Bool) -> (s -> v) -> (Maybe a, v)
minValue state moves term eval
  | term state = (Nothing, eval state)
  | otherwise = minimumBy (comparing snd) $ map util $ moves state False
  where util (action, state') = (Just action, snd $ maxValue state' moves term eval)

minimax :: Ord v => s -> (s -> Bool -> [(a, s)]) -> (s -> Bool) -> (s -> v) -> Maybe a
minimax state moves term eval = fst $ maxValue state moves term eval

getList :: Int -> IO [String]
getList n = replicateM n getLine

type Node = (Char, (Int, Int))
type Board = [[Node]]

diag1 :: Board -> [Node]
diag1 = zipWith (flip (!!)) [0..]

diag2 :: Board -> [Node]
diag2 = reverse . diag1 . reverse

allLines :: Board -> [[Node]]
allLines board = diag1 board : diag2 board : (board ++ transpose board)

isFree :: Node -> Bool
isFree = (== '_') . fst

isPlayer :: Char -> Node -> Bool
isPlayer player = (== player) . fst

isWinLineForPlayer :: Char -> [Node] -> Bool
isWinLineForPlayer = all . isPlayer

isWinLine :: [Node] -> Bool
isWinLine l = isWinLineForPlayer 'X' l || isWinLineForPlayer 'O' l

isTerminalState :: Board -> Bool
isTerminalState board = any isWinLine (allLines board) || all (all (not . isFree)) board

evaluateState :: Board -> Char -> Int
evaluateState state myPlayer
  | any (isWinLineForPlayer myPlayer) ls = 1
  | any (isWinLineForPlayer (otherPlayer myPlayer)) ls = -1
  | otherwise = 0
  where ls = allLines state

getMoves :: Char -> Board -> [((Int, Int), Board)]
getMoves player board = map (\x -> (x, doMove x player board)) $ availableMoves board

doMove :: (Int, Int) -> Char -> Board -> Board
doMove (row, column) player board = modifyListElem board row (\x -> modifyListElem x column (\(_, k) -> (player, k)))

modifyListElem :: [a] -> Int -> (a -> a) -> [a]
modifyListElem l idx modifyFunc = zipWith (\i e -> if i /= idx then e else modifyFunc e) [0..] l

toBoard :: [String] -> Board
toBoard = zipWith (\ r s -> zipWith (\ c v -> (v, (r, c))) [0..] s) [0..]

otherPlayer :: Char -> Char
otherPlayer 'X' = 'O'
otherPlayer 'O' = 'X'

availableMoves :: Board -> [(Int, Int)]
availableMoves = map snd . filter isFree . concat

nextMove :: String -> [String] -> (Int, Int)
nextMove player board =
    let
      myPlayer = head player
      enrichedBoard = toBoard board
      doEval state = evaluateState state myPlayer
      doGetMoves state t = getMoves (if t then myPlayer else otherPlayer myPlayer) state
    in fromJust $ minimax enrichedBoard doGetMoves isTerminalState doEval

main :: IO ()
main = do
    player <- getLine
    board1 <- getList 3
    putStrLn.(\(x, y) -> show x ++ " " ++ show y).nextMove player $ board1
