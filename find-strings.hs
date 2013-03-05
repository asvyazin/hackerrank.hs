import Data.List
import Control.Exception

prefixes :: String -> [String]
prefixes [] = []
prefixes (x:xs) = [x] : (map (x:) $ prefixes xs)

substrings :: String -> [String]
substrings [] = []
substrings l@(x:xs) = (prefixes l) ++ (substrings xs)

uniqueSubstringsMultiple :: [String] -> [String]
uniqueSubstringsMultiple = sort . nub . concat . map substrings

nthElem :: [String] -> Int -> IO String
nthElem ls n = do
  result <- try (evaluate (ls !! (n - 1))) :: IO (Either SomeException String)
  case result of
    Left _ -> return "INVALID"
    Right r -> return r
    
str2Int :: String -> Int
str2Int = read

parseInput input = 
  let
    (l0 : ll0) = lines input
    stringsCount = str2Int l0
    (strings, (l1 : ll1)) = splitAt stringsCount ll0
    numbersCount = str2Int l1
    numbers = map str2Int $ take numbersCount ll1
  in
   (strings, numbers)
   
main = do
  (strings, numbers) <- fmap parseInput getContents
  let
    uniqueSubstrings = uniqueSubstringsMultiple strings
  mapM_ (putStrLn =<<) $ map (nthElem uniqueSubstrings) numbers
