import Control.Applicative
import Control.Monad
import Data.Array
import qualified Data.Set as S

data DfsState a = DfsState { dfsExplored :: S.Set a, dfsStack :: [(a, [a])], dfsExploredPath :: [a] }

data DfsStepResult a =
  DfsFinished [a] [a] |
  DfsUnfinished (DfsState a)

dfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> ([a], [a])
dfs next term start = dfs' (DfsState (S.singleton start) [(start, [start])] [])
  where
    dfs' state = case dfsStep next term state of
      DfsFinished x y -> (x, y)
      DfsUnfinished newState -> dfs' newState

dfsStep :: Ord a => (a -> [a]) -> (a -> Bool) -> DfsState a -> DfsStepResult a
dfsStep _ _ (DfsState { dfsStack = [] }) = error "Empty stack"
dfsStep next term (DfsState { dfsExplored = explored, dfsStack = ((x, xPath):xs), dfsExploredPath = exploredPath })
  | term x = DfsFinished (reverse (x:exploredPath)) (reverse xPath)
  | otherwise =
    let
      nextPoints = filter (`S.notMember` explored) $ next x
      newStack = reverse (map (\y -> (y, y:xPath)) nextPoints) ++ xs
      newExplored = S.union explored (S.fromList nextPoints)
      newState = DfsState newExplored newStack (x:exploredPath)
    in DfsUnfinished newState

getInts :: IO [Int]
getInts = (map read . words) <$> getLine

data Cell = Wall | Pacman | Food | Empty deriving (Eq, Show)

type Point = (Int, Int)
type Board = Array Point Cell

data Output = Output { exploredNodes :: [Point], pathNodes :: [Point] }

charToCell :: Char -> Cell
charToCell '%' = Wall
charToCell 'P' = Pacman
charToCell '.' = Food
charToCell '-' = Empty
charToCell c = error $ "Invalid cell char: " ++ show c

board :: Int -> Int -> [String] -> Board
board rowsCount columnsCount boardLines = array ((0, 0), (rowsCount - 1, columnsCount - 1)) $ do
  (i, boardLine) <- zip [0..] boardLines
  (j, c) <- zip [0..] boardLine
  return ((i, j), charToCell c)

printOutput :: Output -> IO ()
printOutput o = do
  print $ length $ exploredNodes o
  mapM_ printPoint $ exploredNodes o
  print (length (pathNodes o) - 1)
  mapM_ printPoint $ pathNodes o
  where printPoint (r, c) = putStrLn $ show r ++ " " ++ show c

pacmanDfs :: Board -> Point -> Point -> Output
pacmanDfs brd p f = let (explored, path) = dfs next' term' p in Output explored path
  where
    term' x = x == f
    next' x = filter ((/= Wall) . (brd !)) $ neighbours brd x

neighbours :: Board -> Point -> [Point]
neighbours brd (r, c) = filter (inRange (bounds brd)) [(r - 1, c), (r, c - 1), (r, c + 1), (r + 1, c)]

main :: IO ()
main = do
  [pr, pc] <- getInts
  [fr, fc] <- getInts
  [rowsCount, columnsCount] <- getInts
  boardLines <- replicateM rowsCount getLine
  let brd = board rowsCount columnsCount boardLines
  printOutput $ pacmanDfs brd (pr, pc) (fr, fc)
