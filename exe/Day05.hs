{-# LANGUAGE TypeApplications #-}

module Main where

--Base
import Text.Gigaparsec (Parsec, Result(Success), parse,  many)
import Text.Gigaparsec.Char (space, string, letter, endOfLine)
import Text.Gigaparsec.Combinator (sepBy)
import Data.List (sortOn)

-- Mine
import Common (adventOfCode, Input)
import Parsers (parsePositiveInteger, sepStartBy, sepStartEndBy)

type Seed = Int
type Location = Int

data Range = Range {dstart :: Int, sstart :: Int, rlength :: Int} deriving Show
data Map = Map {sourceT :: String, destT :: String, ranges :: [Range]} deriving Show
data Situation = Situation [Seed] [Map] deriving Show
data SeedRange = SeedRange {start :: Int, end :: Int} deriving Show

parseRange :: Parsec Range
parseRange =
  Range <$> (parsePositiveInteger <* space) <*> (parsePositiveInteger <* space) <*> parsePositiveInteger

parseMap :: Parsec Map
parseMap = do
  source <- many letter
  string "-to-"
  dest <- many letter
  string " map:"
  ranges <- sepStartEndBy parseRange endOfLine
  return (Map source dest (sortOn sstart ranges))

parseDay05 :: Parsec Situation
parseDay05 = do
  string "seeds: "
  seeds <- sepBy parsePositiveInteger space
  endOfLine
  maps <- sepStartBy parseMap endOfLine
  return (Situation seeds maps)

-- Part A
applyMap :: Int -> Map -> Int
applyMap s (Map _ _ ras) = applyRanges s ras
  where
    applyRanges :: Int -> [Range] -> Int
    applyRanges x [] = x
    applyRanges x ((Range ds ss rl):rs)
      | ss <= x && x < ss + rl = ds + (x - ss)
      | otherwise              = applyRanges x rs

day05parse :: Input -> Situation
day05parse inp = sit
  where (Success sit) = parse @String parseDay05 inp

day05a :: Situation -> String
day05a (Situation seeds maps) = show (minimum [foldl applyMap s maps | s <- seeds])

-- Part B
seedRangeWithLength :: Int -> Int -> SeedRange
seedRangeWithLength s l = SeedRange s (s + l - 1)

pairify :: [a] -> [(a,a)]
pairify [] = []
pairify [x] = [(x, x)]
pairify (x:y:ys) = (x, y):pairify ys

applyMapToRange :: Map -> SeedRange -> [SeedRange]
applyMapToRange (Map _ _ ras) r = applyRanges r ras
  where
    applyRanges :: SeedRange -> [Range] -> [SeedRange]
    applyRanges x [] = [x]
    applyRanges x@(SeedRange s e) rrs@((Range ds ss rl):rs)
      | ss + rl <= s                = applyRanges x rs                            -- Case 1: need to keep going |ss___|  |s___| 
      | s + l <= ss                 = [x]                                         -- Case 2: not mapped at all since |s___|  |ss___|
      | ss <= s && s + l <= ss + rl = [seedRangeWithLength (ds + s - ss) l]       -- Case 3: nested with |ss__|s___s|__ss|
      | s < ss                      = applyRanges (SeedRange s (ss - 1)) rs       -- Case 4: overlap one |s__|ss__|s___|ss
                                   ++ applyRanges (SeedRange ss (s + l - 1)) rrs
      | otherwise                   = applyRanges (SeedRange s (ss + rl - 1)) rrs -- Case 5: overlap two |ss__|s__|ss__|s
                                   ++ applyRanges (SeedRange (ss + rl) (s + l - 1)) rs
      where l = e - s + 1

day05b :: Situation -> String
day05b (Situation seeds maps) = show . minimum . map start $ srangesN
  where sranges0 = map (uncurry seedRangeWithLength) (pairify seeds)
        srangesN = foldl (\ra m -> concatMap (applyMapToRange m) ra) sranges0 maps

main :: IO ()
main = adventOfCode 5 (day05a . day05parse) (day05b . day05parse)
