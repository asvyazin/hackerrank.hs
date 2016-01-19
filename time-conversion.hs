{-# LANGUAGE OverloadedStrings #-}
module Main (main) where


import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Scientific as S
import qualified Data.Text.IO as T


parseInteger :: (Integral a, Bounded a) => Parser a
parseInteger = do
  mResult <- S.toBoundedInteger <$> scientific
  case mResult of
    Nothing ->
      fail "Expected number"
    Just result ->
      return result


data TimeType
  = AM
  | PM


timeTypeParser :: Parser TimeType
timeTypeParser =
  (string "AM" *> pure AM) <|> (string "PM" *> pure PM)


data Time =
  Time
  { hours :: Int
  , minutes :: Int
  , seconds :: Int
  }


formatTimePart :: Int -> String
formatTimePart p
  | p < 10 = "0" ++ show p
  | otherwise = show p


instance Show Time where
  show tm = formatTimePart (hours tm) ++ ":" ++ formatTimePart (minutes tm) ++ ":" ++ formatTimePart (seconds tm)
  

data TimeAMPM =
  TimeAMPM Time TimeType


timeParser :: Parser TimeAMPM
timeParser = do
  h <- parseInteger
  m <- char ':' *> parseInteger
  s <- char ':' *> parseInteger
  t <- timeTypeParser
  return $ TimeAMPM (Time h m s) t


convertTime :: TimeAMPM -> Time
convertTime (TimeAMPM tm tp) =
  let
    h = hours tm
  in 
   case tp of
     AM ->
       if h == 12
       then tm { hours = 0 }
       else tm
     PM ->
       if h == 12
       then tm
       else tm { hours = 12 + h }


main :: IO ()
main = do
  result <- parseOnly timeParser <$> T.getContents
  case result of
    Left err ->
      fail err
    Right t ->
      print $ convertTime t
