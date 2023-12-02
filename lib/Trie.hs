module Trie where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Control.Applicative ((<|>))

data TrieMap a = Node (Maybe a) (Map.Map Char (TrieMap a)) deriving Show

fromList :: [(String, a)] -> TrieMap a
fromList = foldr insert empty

insert :: (String, a) -> TrieMap a -> TrieMap a
insert ("", x) (Node _ cs)   = Node (Just x) cs  
insert (s:ss, x) (Node e cs) = Node e cs'
  where
    cs' = Map.alter (\old -> do child <- old <|> Just empty
                                return (insert (ss, x) child)) s cs

empty :: TrieMap a
empty = Node Nothing Map.empty

lookup :: String -> TrieMap a -> Maybe a
lookup "" (Node ma _) = ma
lookup (s:ss) (Node _ map) = do
  childTrieMap <- Map.lookup s map
  lookup ss childTrieMap

prefixLookup :: String -> TrieMap a -> Maybe a
prefixLookup "" (Node ma _) = ma
prefixLookup _ (Node j@(Just _) _) = j  
prefixLookup (s:ss) (Node _ map) = do
  childTrieMap <- Map.lookup s map
  prefixLookup ss childTrieMap