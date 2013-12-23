import Control.Applicative
import Data.List (maximumBy)
import Data.Maybe
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

type Color = Char
type Coord = Int
type Point = (Coord, Coord)
type Board = M.Map Point Color

color :: Char -> Maybe Color
color '-' = Nothing
color c = Just c

linesToBoard1 :: [String] -> [(Point, Maybe Color)]
linesToBoard1 ls = do
  (i, s) <- zip [0..] ls
  (j, c) <- zip [0..] s
  return ((i, j), color c)

linesToBoard :: [String] -> [(Point, Color)]
linesToBoard = map (\ (p, Just c) -> (p, c)) . filter (\ (_, c) -> isJust c) . linesToBoard1

readBoard :: IO Board
readBoard = do
  _ <- getLine
  ls <- lines <$> getContents
  return $ M.fromList $ linesToBoard ls

isSameColorPoints :: Board -> Point -> Point -> Bool
isSameColorPoints board p1 p2 = fromMaybe False $ (==) <$> M.lookup p1 board <*> M.lookup p2 board

neighbours :: Point -> [Point]
neighbours (row, column) = [(row - 1, column), (row + 1, column), (row, column - 1), (row, column + 1)]

sameColorCluster :: Board -> Point -> S.Set Point
sameColorCluster board p = wave S.empty [p]
  where wave result [] = result
        wave result (x:xs) = let sameColorNeighbours = filter (\q -> isSameColorPoints board x q && (not $ S.member q result)) $ neighbours x
                             in wave (S.insert x result) (xs ++ sameColorNeighbours)

setHead :: S.Set a -> a
setHead = head . S.toList

clusters :: Board -> [S.Set Point]
clusters board = let points = S.fromList $ M.keys board
                 in iter [] points
                    where iter result notVisited | S.null notVisited = result
                                                 | otherwise = let p = setHead notVisited
                                                                   newCluster = sameColorCluster board p
                                                               in iter (newCluster : result) (notVisited S.\\ newCluster)

main :: IO ()
main = do
  board <- readBoard
  let cs = clusters board
      m = maximumBy (comparing S.size) cs
      (row, column) = setHead m
  putStrLn $ (show row) ++ " " ++ (show column)
