module Main (main) where


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


main :: IO ()
main = do
  str1 <- getLine
  str2 <- getLine
  let s1 = fromList str1
  let s2 = fromList str2
  let s = intersection s1 s2
  let n = length_ s
  print ((length str1 - n) + (length str2 - n))
