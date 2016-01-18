import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Array
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.IO
import System.IO.Error

data DfsState a = DfsState { dfsExplored :: S.Set a, dfsStack :: [(a, [a])], dfsExploredPath :: [a], dfsCurrentPos :: (a, [a]) } deriving (Show)

data DfsStepResult a =
  DfsFinished [a] [a] |
  DfsUnfinished (DfsState a) a

dfs :: (Ord a, Show a) => (a -> [a]) -> (a -> Bool) -> a -> ([a], [a])
dfs next term start = dfs' $ defaultDfsState start
  where
    dfs' state = case dfsStep next term state of
      DfsFinished x y -> (x, y)
      DfsUnfinished newState _ -> dfs' newState

defaultDfsState :: a -> DfsState a
defaultDfsState x =
  DfsState { dfsExplored = S.singleton x
           , dfsStack = []
           , dfsExploredPath = []
           , dfsCurrentPos = (x, [x]) }

dfsStep :: (Ord a, Show a) => (a -> [a]) -> (a -> Bool) -> DfsState a -> DfsStepResult a
dfsStep next term (DfsState { dfsExplored = explored, dfsStack = stack, dfsExploredPath = exploredPath, dfsCurrentPos = (x, xPath) })
  | term x = DfsFinished (reverse (x:exploredPath)) (reverse xPath)
  | otherwise =
    let
      nextXs = filter (`S.notMember` explored) $ next x
      (newCurrentPos@(pos,_):newStack) = map (\y -> (y, y:xPath)) nextXs ++ stack
      newExplored = S.union explored (S.fromList nextXs)
      newState = DfsState newExplored newStack (x:exploredPath) newCurrentPos
    in DfsUnfinished newState pos

type Point = (Int, Int)

data Cell = Wall | Empty | Bot | Exit deriving (Eq, Show, Read)

type Board = M.Map Point Cell

data Direction = DUp | DLeft | DRight | DDown deriving (Show)

showDirection :: Direction -> String
showDirection DUp = "UP"
showDirection DLeft = "LEFT"
showDirection DRight = "RIGHT"
showDirection DDown = "DOWN"

readDirection :: String -> Direction
readDirection "UP" = DUp
readDirection "DOWN" = DDown
readDirection "LEFT" = DLeft
readDirection "RIGHT" = DRight
readDirection s = error $ "Invalid direction: " ++ s

data Game = Game { botPosition :: Point, botDirection :: Direction, board :: Board, gameDfs :: DfsState Point } deriving (Show)

type VisibleBoard = Array Point Cell

charToCell :: Char -> Cell
charToCell '#' = Wall
charToCell '-' = Empty
charToCell 'b' = Bot
charToCell 'e' = Exit
charToCell c = error $ "Invalid cell type: " ++ show c

directionToRotation :: Direction -> Point -> Point
directionToRotation DLeft (x, y) = (-y, x)
directionToRotation DRight (x, y) = (y, -x)
directionToRotation DUp x = x
directionToRotation DDown (x, y) = (-x, -y)

rotateVisibleBoard :: Direction -> VisibleBoard -> VisibleBoard
rotateVisibleBoard direction visibleBoard = ixmap (bounds visibleBoard) (directionToRotation direction) visibleBoard

rotateDirection :: Direction -> Direction -> Direction
rotateDirection DUp direction = direction
rotateDirection DLeft DUp = DRight
rotateDirection DLeft DLeft = DUp
rotateDirection DLeft DDown = DLeft
rotateDirection DLeft DRight = DDown
rotateDirection DRight DUp = DLeft
rotateDirection DRight DRight = DUp
rotateDirection DRight DDown = DRight
rotateDirection DRight DLeft = DDown
rotateDirection DDown DUp = DDown
rotateDirection DDown DLeft = DRight
rotateDirection DDown DRight = DLeft
rotateDirection DDown DDown = DUp

mergeBoard' :: Board -> Point -> VisibleBoard -> Board
mergeBoard' brd (curX, curY) visibleBrd =
  foldl' insertValue brd [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  where insertValue b k@(kX, kY) = M.insert (curX + kX, curY + kY) (visibleBrd ! k) b

mergeBoard :: Game -> VisibleBoard -> Game
mergeBoard g@(Game { board = brd, botPosition = pos }) visibleBrd =
  g { board = mergeBoard' brd pos visibleBrd }

computeDirection :: Point -> Point -> Direction
computeDirection (x1, y1) (x2, y2)
  | x1 == x2 && y1 - y2 == 1 = DUp
  | x1 == x2 && y2 - y1 == 1 = DDown
  | y1 == y2 && x1 - x2 == 1 = DLeft
  | y1 == y2 && x2 - x1 == 1 = DRight
  | otherwise = error $ "Points (" ++ show x1 ++ ", " ++ show y1 ++ ") and (" ++ show x2 ++ ", " ++ show y2 ++ ") are not neighbours"

gameDfsStep :: Game -> DfsStepResult Point
gameDfsStep (Game { board = brd, gameDfs = state }) = dfsStep next term state
  where
    next p = filter (\x -> getCell x /= Wall) $ neighbours p
    term p = getCell p == Exit
    getCell p = brd M.! p
    neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

gameStep :: Game -> Maybe (Direction, Game)
gameStep g@(Game { botPosition = pos, botDirection = dir }) =
  case gameDfsStep g of
    DfsFinished _ _ -> Nothing
    DfsUnfinished newState newPos ->
      let
        newDir = computeDirection pos newPos
      in Just (rotateDirection dir newDir, g { botPosition = newPos, botDirection = newDir, gameDfs = newState })

readVisibleBoard :: IO VisibleBoard
readVisibleBoard = do
  strings <- replicateM 3 getLine
  return $ array ((-1, -1), (1, 1)) $ do
    (y, s) <- zip [-1, 0, 1] strings
    (x, c) <- zip [-1, 0, 1] s
    return ((x, y), charToCell c)

readGameFromFile :: IO (Maybe Game)
readGameFromFile = catch (withFile "game.state" ReadMode doReadGameFromFile)
                   (\e ->
                     if isDoesNotExistError e then return Nothing else throwIO e)
  where
    doReadGameFromFile h = Just <$> (Game <$> readPoint h <*> readDir h <*> readBoard h <*> readDfsState h)
    readPoint h = do
      x <- read <$> hGetLine h
      y <- read <$> hGetLine h
      return (x, y)
    readDir h = readDirection <$> hGetLine h
    doReadList h readElem = do
      n <- (read :: String -> Int) <$> hGetLine h
      replicateM n readElem
    readBoard h =
      M.fromList <$> doReadList h
      (do p <- readPoint h
          c <- read <$> hGetLine h
          return (p, c))
    readPath h = doReadList h (readPoint h)
    readPointAndPath h = do
      p <- readPoint h
      l <- readPath h
      return (p, l)
    readDfsState h = DfsState <$> readDfsExplored h <*> readDfsStack h <*> readPath h <*> readPointAndPath h
    readDfsExplored h = S.fromList <$> readPath h
    readDfsStack h = doReadList h (readPointAndPath h)

defaultGame :: Game
defaultGame =
  let pos = (0, 0)
  in Game { botPosition = pos
          , botDirection = DUp
          , board = M.empty
          , gameDfs = defaultDfsState pos }

readGame :: IO Game
readGame = do
  _ <- getLine
  visibleBoard <- readVisibleBoard
  g <- fromMaybe defaultGame <$> readGameFromFile
  return (mergeBoard g (rotateVisibleBoard (botDirection g) visibleBoard))

writeGame :: Game -> IO ()
writeGame (Game botPos botDir brd gDfs) = withFile "game.state" WriteMode $ \h -> do
  writePoint h botPos
  writeDirection h botDir
  writeBoard h brd
  writeDfsState h gDfs
  where
    writePoint h (x, y) = hPrint h x >> hPrint h y
    writeDirection h d = hPutStrLn h (showDirection d)
    writeBoard h b = do
      hPrint h $ M.size b
      forM_ (M.toList b) $ \(p, c) -> do
        writePoint h p
        hPrint h c
    writeDfsState h (DfsState explored stack exploredPath currentPos) =
      writeDfsExplored h explored >> writeDfsStack h stack >> writeDfsExploredPath h exploredPath >> writeDfsCurrentPos h currentPos
    writeDfsExplored h explored = do
      hPrint h $ S.size explored
      forM_ (S.toList explored) (writePoint h)
    writeDfsStack h stack = do
      hPrint h (length stack)
      forM_ stack $ \(x, l) -> do
        writePoint h x
        hPrint h (length l)
        forM_ l (writePoint h)
    writeDfsExploredPath h exploredPath = do
      hPrint h (length exploredPath)
      forM_ exploredPath (writePoint h)
    writeDfsCurrentPos h (p, l) = do
      writePoint h p
      hPrint h (length l)
      forM_ l (writePoint h)
  
main :: IO ()
main = do
  g <- readGame
  let (step, newG) = fromJust $ gameStep g
  writeGame newG
  putStrLn $ showDirection step
