{-# LANGUAGE TupleSections #-}

module Main where
-- Base
import Data.Char (isNumber)
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)

-- Mine
import Common (adventOfCode, Line, Input)
import String (findSubstring)

digits :: [(String, Int)]
digits = [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six",6), ("seven", 7), ("eight", 8), ("nine", 9), 
          ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9)] 

day01 :: (Line -> Int) -> (Input -> String)
day01 extractNumber = show . sum . map extractNumber . lines

extractNumberA :: Line -> Int
extractNumberA cs = read [head digs, last digs] :: Int
  where digs = filter isNumber cs

extractNumberB :: Line -> Int
extractNumberB line = firstDigit * 10 + lastDigit
  where
    digitOccurs :: [(Int, Int)]
    digitOccurs = concat [map (, dig) (findSubstring str line) | (str, dig) <- digits]
    
    firstDigit :: Int 
    firstDigit = snd . minimumBy (comparing fst) $ digitOccurs 
    
    lastDigit :: Int
    lastDigit = snd . maximumBy (comparing fst) $ digitOccurs

main :: IO ()
main = adventOfCode 1 (day01 extractNumberA) (day01 extractNumberB)
