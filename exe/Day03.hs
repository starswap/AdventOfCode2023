module Main where

-- Base
import Data.Array (Array, bounds, (!))
import Data.Char (isDigit)

-- Mine
import Common (adventOfCode, Input, arrayParse)

type Map = Array (Int, Int) Char

day03a :: Map -> String
day03a = show . sum . findNumbers (1, 1) "" False

day03b :: Map -> String
day03b = show . sum . findGears (1, 1)

isSymbol :: Char -> Bool
isSymbol x = x /= '.' && not (isDigit x)

findNumbers :: (Int, Int) -> String -> Bool -> Map -> [Int]
findNumbers (r, c) digString doesCount m
  | r == h && c == w + 1           = []
  | c == w + 1 || not (isDigit ch) = accumNumber digString doesCount (findNumbers next  "" False m)
  | otherwise                      = findNumbers next (ch:digString) (doesCount || check (r, c) m) m
  where 
    (_, (h, w)) = bounds m
    ch = m ! (r, c)

    accumNumber :: String -> Bool -> [Int] -> [Int]
    accumNumber "" _ xs    = xs
    accumNumber _ False xs = xs
    accumNumber sn True xs = (read (reverse sn) :: Int):xs

    next = if c == w + 1 then (r + 1, 1) else (r, c + 1)

check :: (Int, Int) -> Map -> Bool
check (r, c) m = any (checkOne) [(r + dr, c + dc)| (dr, dc) <- ds]
  where
    ds :: [(Int, Int)]
    ds = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)] 
        
    checkOne :: (Int, Int) -> Bool
    checkOne (r, c) = (r <= h) && (r >= 1) && (c <= w) && (c >= 1) && (isSymbol (m ! (r, c)))
    
    (_, (h, w)) = bounds m

findGears :: (Int, Int) -> Map -> [Int]
findGears (r, c) m 
  | r == h && c == w + 1 = []  
  | m ! (r, c) == '*'    = (if length adjNumbers == 2 then ....() else) ++ findGears next m 
  | otherwise            = findGears next m
  where
    (_, (h, w)) = bounds m
    adj = getNumbers (r, c) m
    next = if c == w + 1 then (r + 1, 1) else (r, c + 1)
    adjNumbers = numbersAround (r, c) m

numbersAround :: (Int, Int) -> Map -> [Int]

-- find numbers around -- left, right, if middle is empty then check up left and up right separately otherwise check as one effectively. same for below

inp :: String
inp = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

main :: IO ()
main = adventOfCode 3 (day03a . arrayParse) (day03b . day03Parse)

