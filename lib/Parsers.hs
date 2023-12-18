{-# LANGUAGE TypeApplications #-}

module Parsers where

import Text.Gigaparsec (some, Parsec, Result(Success), parse)
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Combinator (sepBy, sepEndBy)

import Common (Input)

parsePositiveInteger :: Parsec Int
parsePositiveInteger = fmap read (some digit)

sepStartBy :: Parsec a -> Parsec sep -> Parsec [a]
sepStartBy p sep = sep *> sepBy p sep

sepStartEndBy :: Parsec a -> Parsec sep -> Parsec [a]
sepStartEndBy p sep = sep *> sepEndBy p sep

parseWellFormed :: Parsec a -> Input -> a
parseWellFormed p inp = result
  where (Success result) = parse @String p inp
