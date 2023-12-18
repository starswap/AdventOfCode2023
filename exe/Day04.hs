module Main where

--Base
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map (Map)
import Text.Gigaparsec (Parsec, many, some)
import Text.Gigaparsec.Char (char, space, string)
import Text.Gigaparsec.Combinator (sepBy, sepEndBy)

-- Mine
import Common (adventOfCode, Input)
import Parsers (parsePositiveInteger, parseWellFormed)

type CardId = Int
type Score = Int

data Card = Card {cardId :: CardId, winning :: S.Set Int, have :: S.Set Int} deriving Show

parseCard :: Parsec Card
parseCard = do
    string "Card"
    many space
    cardId <- parsePositiveInteger
    string ":"
    some space
    winningNumbers <- sepEndBy parsePositiveInteger (some space)
    char '|'
    some space
    haveNumbers <- sepBy parsePositiveInteger (some space)
    return (Card cardId (S.fromList winningNumbers) (S.fromList haveNumbers))

day04parse :: Input -> [Card]
day04parse = map (parseWellFormed parseCard) . lines

day04a :: [Card] -> Int
day04a = sum . map score

match :: Card -> Int
match (Card _ winners haves) = length (S.intersection winners haves)

score :: Card -> Score
score c
  | matches == 0 = 0
  | otherwise    = 2 ^ (matches - 1)
  where matches = match c

day04b :: [Card] -> Int
day04b cs = sum . map (M.foldr (+) 0) $ cardsNs
  where
    cardsNs :: [Map CardId Int]
    cardsNs = takeWhile (not . M.null) (iterate newCardsOneStep cards0)

    cards0:: Map CardId Int
    cards0 = M.fromList [(cardId c, 1) | c <- cs]
    
    matches :: Map CardId Int
    matches = M.fromList [(cardId c, match c) | c <- cs]

    newCardsOneStep :: Map CardId Int -> Map CardId Int
    newCardsOneStep = M.foldrWithKey oneStepOneCard M.empty

    oneStepOneCard :: CardId -> Int -> Map CardId Int -> Map CardId Int
    oneStepOneCard i a m = foldr (M.alter incr) m [i + 1 .. i + matches M.! i]
      where
        incr :: Maybe Int -> Maybe Int
        incr (Just n) = Just (n + a)
        incr Nothing  = Just a
    
main :: IO ()
main = adventOfCode 4 day04parse day04a day04b
