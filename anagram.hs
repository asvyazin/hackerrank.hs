module Main (main) where


import Control.Applicative
import Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M


type MultiSet a = M.Map a Int


fromList :: Ord a => [a] -> MultiSet a
fromList =
  L.foldl' (flip insert) M.empty
  

insert :: Ord a => a -> MultiSet a -> MultiSet a
insert el =
  M.insertWith (+) el 1


intersection :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersection =
  M.intersectionWith min


length_ :: MultiSet a -> Int
length_ =
  M.foldl' (+) 0


solve :: String -> Int
solve str =
  let
    n = length str

    result
      | odd n = -1
      | otherwise =
        let
          n2 = n `div` 2
          (str1, str2) = L.splitAt n2 str
          s = intersection (fromList str1) (fromList str2)
        in
          length str1 - length_ s
  in result


main :: IO ()
main = do
  t <- (read :: String -> Int) <$> getLine
  replicateM_ t $ do
    str <- getLine
    print $ solve str
