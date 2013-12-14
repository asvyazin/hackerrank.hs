module Main (main) where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type WaveState a = ([a], S.Set a)

setInsertAll :: Ord a => [a] -> S.Set a -> S.Set a
setInsertAll l s = foldl (\s1 e -> S.insert e s1) s l

tryHead :: [a] -> Maybe (a, [a])
tryHead [] = Nothing
tryHead (x:xs) = Just (x, xs)

waveState :: Ord a => (a -> [a]) -> (a -> Bool) -> StateT (WaveState a) Maybe a
waveState neighbours target = do
  (q, visited) <- get
  (x, xs) <- lift $ tryHead q
  if target x
    then return x
    else do
    let notVisitedNeighbours = filter (`S.notMember` visited) $ neighbours x
    let newQ = xs ++ notVisitedNeighbours
    let newVisited = setInsertAll notVisitedNeighbours visited
    put (newQ, newVisited)
    waveState neighbours target

wave :: Ord a => (a -> [a]) -> (a -> Bool) -> [a] -> Maybe a
wave neighbours target starts = evalStateT (waveState neighbours target) (starts, S.fromList starts)

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

neighboursAll :: Point -> [Point]
neighboursAll (row, column) = [(x, column) | x <- filter inBorders [row - 1, row + 1]] ++ [(row, y) | y <- filter inBorders [column - 1, column + 1]]
  where inBorders i = i >= 0 && i < 10

neighboursToHit :: Board -> Point -> [Point]
neighboursToHit board p = filter possibleToHit $ neighboursAll p
  where possibleToHit neighbour = case M.lookup neighbour board of
          Nothing -> False
          Just Unknown -> True
          Just Hit -> True
          _ -> False

unknownCell :: Board -> Point -> Bool
unknownCell board p = case M.lookup p board of
  Nothing -> False
  Just Unknown -> True
  _ -> False

tryFinishOff :: Board -> Maybe Point
tryFinishOff board = let hitCells = M.keys $ M.filter (== Hit) board in
  wave (neighboursToHit board) (unknownCell board) hitCells

moveToFirstUnknown :: Board -> Maybe Point
moveToFirstUnknown board = Just $ head $ M.keys $ M.filter (== Unknown) board

tryMove :: Board -> Maybe Point
tryMove board = tryFinishOff board <|> moveToFirstUnknown board

main :: IO ()
main = do
  board <- readBoard
  let (row, column) = fromJust $ tryMove board
  putStrLn $ (show row) ++ " " ++ (show column)
