
module Main where

--Base
import Text.Gigaparsec (Parsec, some)
import Text.Gigaparsec.Char (letterOrDigit, space)
import Data.List (sortOn, sort)
import Data.Ord (comparing)
import qualified Data.Set as S 

-- Mine
import Common (adventOfCode, Input)
import Parsers (parsePositiveInteger, parseWellFormed)

type Bid = Int
type Rank = Int
newtype Hand = Hand [Card] deriving (Eq, Show)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)
data Game = Game {hand :: Hand, bid :: Bid} deriving Show

readCard :: Char -> Card
readCard '2' = Two
readCard '3' = Three
readCard '4' = Four
readCard '5' = Five
readCard '6' = Six
readCard '7' = Seven
readCard '8' = Eight
readCard '9' = Nine
readCard 'T' = Ten
readCard 'J' = Jack
readCard 'Q' = Queen
readCard 'K' = King
readCard 'A' = Ace
readCard _   = error "Malformed Card"

instance Show Card where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "T"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Ord Hand where
  compare ha@(Hand h) hb@(Hand i) = (comparing classifyHand ha hb) <> (compare h i)

classifyHand :: Hand -> HandType
classifyHand (Hand [])                                          = error "Empty hand!"
classifyHand (Hand h@(c:cs))
  | all (== c) cs                                               = FiveOfAKind
  | all (== s) [t, u, v] || all (== t) [u, v, w]                = FourOfAKind  
  | unique == 2                                                 = FullHouse
  | all (== s) [t, u] || all (== t) [u, v] || all (== u) [v, w] = ThreeOfAKind
  | unique == 3                                                 = TwoPair
  | unique == 4                                                 = OnePair
  | otherwise                                                   = HighCard
  where (s:t:u:v:w:[]) = sort h
        unique         = length (S.fromList h)

parseCard :: Parsec Card
parseCard = readCard <$> (letterOrDigit)

parseGame :: Parsec Game 
parseGame = Game <$> ((Hand <$> some parseCard) <* (space)) <*> (parsePositiveInteger)

parseDay07 :: Input -> [Game] 
parseDay07 = map (parseWellFormed parseGame) . lines 

getWinning :: (Rank, Game) -> Int
getWinning (rank, Game hand bid) = bid * rank

day07a :: [Game] -> Int
day07a games = sum . map getWinning . zip [1..] . sortOn hand $ games

day07b :: [Game] -> Int
day07b = undefined

inp = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

main :: IO ()
main = adventOfCode 7 parseDay07 day07a day07b
