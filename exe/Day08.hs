module Main where

--Base
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Gigaparsec (Parsec, some, many, (<|>))
import Text.Gigaparsec.Char ( char, endOfLine, string, upper )
import Text.Gigaparsec.Combinator (sepBy)

-- Mine
import Common (adventOfCode)
import Parsers (parseWellFormed)

data Instruction = L | R deriving Show
type Instructions = [Instruction]
type Place = String
type Tree = M.Map Place (Place, Place)
data Situation = Situation Instructions Tree deriving Show

parseInstruction :: Parsec Instruction
parseInstruction = (L <$ char 'L') <|> (R <$ char 'R')

parseNode :: Parsec (Place, (Place, Place))
parseNode = do
    f <- some upper
    string " = ("
    s <- some upper
    string ", "
    t <- some upper
    string ")"
    return (f, (s, t))

day08parse :: Parsec Situation
day08parse = do
    instrs <- many parseInstruction
    endOfLine
    endOfLine
    nodes <- sepBy parseNode endOfLine
    return (Situation instrs (M.fromList nodes))


nextPlace :: Tree -> Instruction -> Place -> Place
nextPlace t L pl = fst (t M.! pl)
nextPlace t R pl = snd (t M.! pl)

countSteps :: (Place -> Bool) -> Tree -> Instructions -> Instructions -> Place -> Integer
countSteps p t oIs [] pl = countSteps p t oIs oIs pl
countSteps p t oIs (i:iis) pl
  | p pl      = 0
  | otherwise = 1 + countSteps p t oIs iis (nextPlace t i pl)

day08a :: Situation -> Integer
day08a (Situation is tr) = countSteps (== "ZZZ") tr is is "AAA"

recordCycles :: Tree -> Instructions -> Instructions -> M.Map Place Int -> Int -> Place-> (Int, Int)
recordCycles t oIs [] seenAtEnd i pl
  | M.member pl seenAtEnd                               = (i - seenAtEnd M.! pl, 0)
  | otherwise                                           = recordCycles t oIs oIs (M.insert pl i seenAtEnd) i pl
recordCycles t oIs (ins:inss) seenAtEnd i pl@[_ ,_,'Z'] = i <$ recordCycles t oIs inss seenAtEnd (i + 1) (nextPlace t ins pl)
recordCycles t oIs (ins:inss) seenAtEnd i pl            = recordCycles t oIs inss seenAtEnd (i + 1) (nextPlace t ins pl)

-- This works because you are supposed to recognise that the input is constructed such that:
--    - Every A goes to exactly 1 Z
--    - It takes N_i instructions for the ith A to go to that Z, after which point the ghost returns to that same Z on a cycle of N_i instructions.
-- I used the recordCycles function to determine this.
-- I'm of the opinion that this is not a particularly constructive problem
day08b :: Situation -> Integer
day08b (Situation is tr) = foldl lcm 1 zs
  where
    startNodes = filter (\k -> k !! 2 == 'A') (M.keys tr)
    zs         = map (countSteps (\s -> s !! 2 == 'Z') tr is is) startNodes

main :: IO ()
main = adventOfCode 8 (parseWellFormed day08parse) day08b day08b
