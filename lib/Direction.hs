module Direction where

data Direction = R | D | L | U deriving (Eq, Ord, Enum, Show)

deltas :: [(Int, Int)]
deltas = [(0, 1), (1, 0), (-1, 0), (0, -1)]

delta :: Direction -> (Int, Int)
delta L = (0, -1)
delta R = (0, 1)
delta U = (-1, 0)
delta D = (1, 0)

opposite :: Direction -> Direction
opposite L = R
opposite R = L
opposite D = U
opposite U = D

leftTurn :: Direction -> Direction
leftTurn x = toEnum $ (fromEnum x - 1) `mod` 4

rightTurn :: Direction -> Direction
rightTurn x = toEnum $ (fromEnum x + 1) `mod` 4
