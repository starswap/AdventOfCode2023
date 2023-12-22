module Deque where

newtype Deque a = Deque [a] [a]

cons :: a -> Deque a -> Deque a
cons x (Deque xs sy) = Deque (x:xs) sy

snoc :: a -> Deque a -> Deque a
snoc y (Deque xs sy) = Deque xs (y:sy)

uncons :: Deque a -> (a, Deque a)
uncons (Deque []) = error "Pop front from empty deque"
uncons (Deque (x:xs) sy) = 

unsnoc :: Deque a -> (a, Deque a)
unsnoc 

empty :: Deque a
empty = Deque [a]

null :: Deque a -> Bool
null (Deque xs) = null xs
