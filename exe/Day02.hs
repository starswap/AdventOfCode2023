module Main where

-- Base
import Text.Gigaparsec (Parsec, atomic, (<|>), many)
import Text.Gigaparsec.Char (string, digit, char)
import Text.Gigaparsec.Combinator (sepBy)

-- Mine
import Common (adventOfCode, Input)
import Parsers (parseWellFormed)

data Game = Game {gameId :: Int, bags :: [Bag]} deriving Show

data Bag = Bag {red :: Int, green :: Int, blue :: Int} deriving (Eq, Show)

instance Ord Bag where
  (Bag r g b) <= (Bag r' g' b') = r <= r' && g <= g' && b <= b'

(+++) :: Bag -> Bag -> Bag
Bag r g b +++ Bag r' g' b' = Bag (r + r') (g + g') (b + b')

maxBag :: Bag -> Bag -> Bag
maxBag (Bag r g b) (Bag r' g' b') = Bag (max r r') (max g g') (max b b')

power :: Bag -> Int
power (Bag r g b) = r * g * b

empty :: Bag
empty = Bag 0 0 0

target :: Bag
target = Bag 12 13 14

parsePositiveInteger :: Parsec Int
parsePositiveInteger = fmap read (many digit)

parseColourBag :: (Int -> Bag) -> String -> Parsec Bag
parseColourBag bagConstructor colourName = bagConstructor <$> (parsePositiveInteger <* char ' ' <* string colourName)

parseRedBag :: Parsec Bag
parseRedBag = parseColourBag (\r -> Bag r 0 0) "red"

parseGreenBag :: Parsec Bag
parseGreenBag = parseColourBag (\g -> Bag 0 g 0) "green"

parseBlueBag :: Parsec Bag
parseBlueBag = parseColourBag (Bag 0 0) "blue"

parseAnyBag :: Parsec Bag
parseAnyBag = atomic parseRedBag <|> atomic parseGreenBag <|> atomic parseBlueBag

parseSample :: Parsec Bag
parseSample = foldl (+++) empty <$> sepBy parseAnyBag (string ", ")

parseSamples :: Parsec [Bag]
parseSamples = sepBy parseSample (string "; ")

parseGame :: Parsec Game
parseGame = do
  string "Game "
  gameId <- parsePositiveInteger
  string ": "
  Game gameId <$> parseSamples

day02Parse :: Input -> [Game]
day02Parse input = map (parseWellFormed parseGame) (lines input)

day02a :: [Game] -> Int
day02a games = sum [gameId | Game gameId bags <- games, all (<= target) bags]

day02b :: [Game] -> Int
day02b games = sum [power (foldl maxBag empty bags) | (Game _ bags) <- games]

main :: IO ()
main = adventOfCode 2 day02Parse day02a day02b

