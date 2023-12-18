
module Main where

--Base
import Text.Gigaparsec (Parsec,  many, some)
import Text.Gigaparsec.Char (space, string, endOfLine, digit)

-- Mine
import Common (adventOfCode)
import Parsers (parsePositiveInteger, sepStartBy, parseWellFormed)

data Race = Race {time :: Int, distance :: Int} deriving Show

parseDay06a :: Parsec [Race]
parseDay06a = do
  string "Time:"
  times <- sepStartBy parsePositiveInteger (many space)
  endOfLine
  string "Distance:"
  dists <- sepStartBy parsePositiveInteger (many space)
  return (zipWith Race times dists)

parseDay06b :: Parsec [Race]
parseDay06b = do
  string "Time:"
  times <- sepStartBy (some digit) (some space)
  endOfLine
  string "Distance:"
  dists <- sepStartBy (some digit) (some space)
  let t = read (concat times) :: Int
  let d = read (concat dists) :: Int
  return [Race t d]

day06 :: [Race] -> Int
day06 = foldr ((*) . numberBeatRecord) 1

numberBeatRecord :: Race -> Int
numberBeatRecord (Race t d) = length [v * (t - v) | v <- [0..t],  v * (t - v) > d]

main :: IO ()
main = adventOfCode 5 id (day06 . parseWellFormed parseDay06a) (day06 . parseWellFormed parseDay06b)
