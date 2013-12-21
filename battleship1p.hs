module Battleship1p (main) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.List (find)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Point = (Int, Int)
data CellType = Hit | Miss | Destroyed | Unknown deriving (Eq, Show)
type Board = M.Map Point CellType

charToCellType :: Char -> CellType
charToCellType 'h' = Hit
charToCellType 'm' = Miss
charToCellType 'd' = Destroyed
charToCellType '-' = Unknown

linesToBoard :: [String] -> [(Point, CellType)]
linesToBoard ls = do
  (i, s) <- zip [0..] ls
  (j, c) <- zip [0..] s
  return ((i, j), charToCellType c)

readBoard :: IO Board
readBoard = do
  _ <- getLine
  ls <- lines <$> getContents
  return $ M.fromList $ linesToBoard ls

allNeighbours2 :: Board -> Point -> [[Point]]
allNeighbours2 board (row, column) = [[(row - 1, column), (row - 2, column)],
                                      [(row, column + 1), (row, column + 2)],
                                      [(row + 1, column), (row + 2, column)],
                                      [(row, column - 1), (row, column - 2)]]

tryFinishOff2 :: Board -> Maybe Point
tryFinishOff2 board = find (\p -> any (all $ cellHasType Hit board) $ allNeighbours2 board p) $ cellsByType board Unknown

allNeighbours :: Board -> Point -> [Point]
allNeighbours board (row, column) = [(row - 1, column), (row, column + 1), (row + 1, column), (row, column - 1)]

tryFinishOff :: Board -> Maybe Point
tryFinishOff board = find (any (cellHasType Hit board) . allNeighbours board) $ cellsByType board Unknown

cellHasType :: CellType -> Board -> Point -> Bool
cellHasType cellType board p = case M.lookup p board of
  Nothing -> False
  Just x -> x == cellType

carrierHuntingPoints :: [Point]
carrierHuntingPoints = [(x, x) | x <- [0..9]] ++ [(x, 5 + x) | x <- [0..4]] ++ [(5 + x, x) | x <- [0..4]]

battleshipHuntingPoints :: [Point]
battleshipHuntingPoints = [(0, 9), (9, 0)] ++ [(x + 2, x) | x <- [0..7]] ++ [(x, x + 2) | x <- [0..7]]

cruiserHuntingPoints :: [Point]
cruiserHuntingPoints = [(x + 7, x) | x <- [0..2]] ++ [(x, x + 7) | x <- [0..2]]

destroyerHuntingPoints :: [Point]
destroyerHuntingPoints = [(x + 4, x) | x <- [0..5]] ++ [(x, x + 4) | x <- [0..5]]

submarineHuntingPoints :: [Point]
submarineHuntingPoints = [(x, y) | x <-[0..9], y <- [0..9]]

tryHuntShip :: [Point] -> Board -> Maybe Point
tryHuntShip huntingPoints board = find (cellHasType Unknown board) huntingPoints

cellsByType :: Board -> CellType -> [Point]
cellsByType board cellType = M.keys $ M.filter (== cellType) board

tryMove :: Board -> Maybe Point
tryMove board = tryFinishOff2 board <|>
                tryFinishOff board <|>
                tryHuntShip carrierHuntingPoints board <|>
                tryHuntShip battleshipHuntingPoints board <|>
                tryHuntShip cruiserHuntingPoints board <|>
                tryHuntShip destroyerHuntingPoints board <|>
                tryHuntShip submarineHuntingPoints board

main :: IO ()
main = do
  board <- readBoard
  let (row, column) = fromJust $ tryMove board
  putStrLn $ (show row) ++ " " ++ (show column)
