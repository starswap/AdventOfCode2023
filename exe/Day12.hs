module Main where

-- Mine
import Common (adventOfCode)
import Parsers (parseWellFormed, parsePositiveInteger)
import Array (tabulate, arrayTake)

-- Base
import Text.Gigaparsec (Parsec, many)
import Text.Gigaparsec.Char (space, oneOf, char)
import Text.Gigaparsec.Combinator (sepBy)
import qualified Data.Set as S
import Data.Array
import Data.List (intercalate)

data SpringConfig = SpringConfig [Char] [Int]

day12parse :: Parsec SpringConfig 
day12parse = do
  letters <- many (oneOf (S.fromList "?.#"))
  space
  numbers <- sepBy parsePositiveInteger (char ',')
  return (SpringConfig letters numbers)

doDP :: SpringConfig -> Int
doDP (SpringConfig lettersList numbersList) = table ! (0, 0)
  where 
    m = length lettersList
    n = length numbersList
    
    letters = (listArray (0, m - 1) lettersList)
    numbers = (listArray (0, n - 1) numbersList)

    table = tabulate ((0, 0), (m + 1, n)) (uncurry memo)

    memo :: Int -> Int -> Int
    memo letIdx numIdx
      | letIdx >= m              = if numIdx < n then 0 else 1
      | numIdx >= n && ch == '#' = 0
      | numIdx >= n && ch == '?' = dotResult 
      | ch == '.'                = dotResult
      | ch == '#'                = hashResult
      | ch == '?'                = dotResult + hashResult
      where ch = letters ! letIdx
            g  = numbers ! numIdx
            hashing = arrayTake letters (letIdx, letIdx + g - 1)
            dotResult = table ! (letIdx + 1, numIdx)
            hashResult = if letIdx + g - 1 >= m ||                            -- Group doesn't fit inside the string
                            any (== '.') hashing ||                           -- We can't place a group here because there are .s
                            (letIdx + g < m && letters ! (letIdx + g) == '#') -- We can't place a group of this size here because there are too many #s 
                           then 0
                           else table ! (letIdx + g + 1, numIdx + 1) -- skip one space because we can't place two groups next to each other

unfoldSprings :: SpringConfig -> SpringConfig
unfoldSprings (SpringConfig letters numbers) = SpringConfig newLetters newNumbers
  where 
    newNumbers = concat (replicate 5 numbers)
    newLetters = intercalate "?" (replicate 5 (letters)) 

day12a :: [SpringConfig] -> Int
day12a = sum . map doDP

day12b :: [SpringConfig] -> Int
day12b = sum . map (doDP . unfoldSprings)

main :: IO ()
main = adventOfCode 12 (map (parseWellFormed day12parse) . lines) day12a day12b
