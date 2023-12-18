
module Main where

--Base
import Text.Gigaparsec (Parsec, some)
import Text.Gigaparsec.Char (letterOrDigit, space)
import Data.List (sortOn, sort, maximumBy, group)
import Data.Ord (comparing)
import qualified Data.Set as S 

-- Mine
import Common (adventOfCode, Input)
import Parsers (parsePositiveInteger, parseWellFormed)

type Bid = Int
type Rank = Int
newtype Hand = Hand [Card] deriving (Eq, Show)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord)
data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Eq, Ord, Show)
data Game = Game {hand :: Hand, bid :: Bid} deriving Show

readCardA :: Char -> Card
readCardA '2' = Two
readCardA '3' = Three
readCardA '4' = Four
readCardA '5' = Five
readCardA '6' = Six
readCardA '7' = Seven
readCardA '8' = Eight
readCardA '9' = Nine
readCardA 'T' = Ten
readCardA 'J' = Jack
readCardA 'Q' = Queen
readCardA 'K' = King
readCardA 'A' = Ace
readCardA _   = error "Malformed Card"

readCardB :: Char -> Card
readCardB 'J' = Joker
readCardB x   = readCardA x

instance Show Card where
  show Joker = "J"
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

listReplace :: (Eq a) => a -> a -> [a] -> [a]
listReplace _ _ [] = []
listReplace s t (x:xs)
  | s == x    = t:listReplace s t xs
  | otherwise = x:listReplace s t xs  

classifyHand :: Hand -> HandType
classifyHand (Hand [])                                          = error "Empty hand!"
classifyHand (Hand h@(c:cs))
  | all (== c) cs                                               = FiveOfAKind
  | S.member Joker (S.fromList h)                               = classifyHand (Hand (listReplace Joker mostCommonCard h))
  | all (== s) [t, u, v] || all (== t) [u, v, w]                = FourOfAKind
  | unique == 2                                                 = FullHouse
  | all (== s) [t, u] || all (== t) [u, v] || all (== u) [v, w] = ThreeOfAKind
  | unique == 3                                                 = TwoPair
  | unique == 4                                                 = OnePair
  | otherwise                                                   = HighCard
  where sorted@(s:t:u:v:w:[]) = sort h
        unique         = length (S.fromList h)
        mostCommonCard = fst . maximumBy (comparing snd) $ [(c, length group) | group@(c:cs) <- group (filter (/= Joker) sorted)]

parseCard :: (Char -> Card) -> Parsec Card
parseCard readCard = readCard <$> (letterOrDigit)

parseGame :: (Char -> Card) -> Parsec Game 
parseGame readCard = Game <$> ((Hand <$> some (parseCard readCard)) <* (space)) <*> (parsePositiveInteger)

parseDay07 :: (Char -> Card) -> Input -> [Game] 
parseDay07 readCard = map (parseWellFormed (parseGame readCard)) . lines 

getWinning :: (Rank, Game) -> Int
getWinning (rank, Game hand bid) = bid * rank

day07 :: [Game] -> Int
day07 games = sum . map getWinning . zip [1..] . sortOn hand $ games

inp = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

main :: IO ()
main = adventOfCode 7 id (day07 . parseDay07 readCardA) (day07 . parseDay07 readCardB)
