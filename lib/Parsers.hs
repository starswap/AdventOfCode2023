module Parsers where

import Text.Gigaparsec (some, Parsec)
import Text.Gigaparsec.Char (digit)
import Text.Gigaparsec.Combinator (sepBy, sepEndBy)

parsePositiveInteger :: Parsec Int
parsePositiveInteger = fmap read (some digit)

sepStartBy :: Parsec a -> Parsec sep -> Parsec [a]
sepStartBy p sep = sep *> sepBy p sep

sepStartEndBy :: Parsec a -> Parsec sep -> Parsec [a]
sepStartEndBy p sep = sep *> sepEndBy p sep
