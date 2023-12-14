module Day03 where

-- Base
import Control.Applicative ((<|>))
import Data.Array (Array, bounds, (!))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, catMaybes)

-- Mine
import Common (adventOfCode, arrayParse)

type Map = Array (Int, Int) Char

isSymbol :: Char -> Bool
isSymbol x = x /= '.' && not (isDigit x)

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (h, w) (r, c) = r <= h && r >= 1 && c <= w && c >= 1

inBoundsAndDigit :: (Int, Int) -> Map -> Bool
inBoundsAndDigit (r, c) m = inBounds (h, w) (r, c) && isDigit (m ! (r, c))
  where (_, (h, w)) = bounds m

day03a :: Map -> String
day03a = show . sum . findNumbers (1, 1) "" False

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
check (r, c) m = any checkOne [(r + dr, c + dc)| (dr, dc) <- ds]
  where
    ds :: [(Int, Int)]
    ds = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    checkOne :: (Int, Int) -> Bool
    checkOne (r, c) = inBounds (h, w) (r, c) && isSymbol (m ! (r, c))

    (_, (h, w)) = bounds m

day03b :: Map -> String
day03b = show . sum . findGears (1, 1)

findGears :: (Int, Int) -> Map -> [Int]
findGears (r, c) m
  | r == h + 1 && c == 1 = []
  | m ! (r, c) == '*'    = if length adjNumbers == 2 then (x * y):rest else rest
  | otherwise            = rest
  where
    (_, (h, w)) = bounds m
    adjNumbers = numbersAround (r, c) m
    (x : y : _) = adjNumbers
    rest = findGears (if c == w then (r + 1, 1) else (r, c + 1)) m

numbersAround :: (Int, Int) -> Map -> [Int]
numbersAround (r, c) m = map read (catMaybes [left, right]) ++ top ++ bot
  where left  = getLeft (r, c - 1) m
        right = getRight (r, c + 1) m
        topL  = getLeft (r - 1, c - 1) m
        topR  = getRight (r - 1, c + 1) m
        botL  = getLeft (r + 1, c - 1) m
        botR  = getRight (r + 1, c + 1) m
        top   = if inBoundsAndDigit (r - 1, c) m
                  then combineLMandR topL (m ! (r - 1, c)) topR
                  else landRToList topL topR
        bot   = if inBoundsAndDigit (r + 1, c) m
                  then combineLMandR botL (m ! (r + 1, c)) botR
                  else landRToList botL botR
        
        combineLMandR :: Maybe String -> Char -> Maybe String -> [Int]
        combineLMandR left chm right = [read (fromMaybe "" left ++ [chm] ++ fromMaybe "" right)]

        landRToList :: Maybe String -> Maybe String -> [Int] 
        landRToList l r = map read (catMaybes [l, r])

getLeft :: (Int, Int) -> Map -> Maybe String
getLeft (r, c) m = fmap reverse (extractNumber (r, c) (0, -1) m)

getRight :: (Int, Int) -> Map -> Maybe String
getRight (r, c) = extractNumber (r, c) (0, 1)

extractNumber :: (Int, Int) -> (Int, Int) -> Map -> Maybe String
extractNumber (r, c) (dr, dc) m
  | not (inBoundsAndDigit (r, c) m) = Nothing
  | otherwise                       = fmap (d:) (extractNumber (r + dr, c + dc) (dr, dc) m <|> Just "")
  where d = m ! (r, c)

main :: IO ()
main = adventOfCode 3 (day03a . arrayParse) (day03b . arrayParse)
