{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (parse_openlects) where

import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Token

-- NOTICE
-- some modification required before copying!
-- 1. there should be no double quote charactor (") within excel file
--    erase them or replace it with another (like (``))
--    additionally, I did erased meaningless column
--    (probably being there for padding) for ease of parsing
--    TODO we may use getContents instead, for memory efficiency
-- 2. a file assumed to have only one school category.
--    it can be extended in case of need

-- The only public method of this module
parse_openlects file = readFile file >>= return . parse openlects file


-- some parser combinators
except :: Stream s m Char => Char -> ParsecT s u m [Char]
except c = many $ satisfy (/=c) 

followedBy :: Stream s m Char
           => ParsecT s u m t -> a -> ParsecT s u m a
followedBy p a = p >> return a

-- read openlects file
openlects = table

-- read whole table, returns its content
table = header >>= title >>= content

-- read heading of table (name of table etc..) returns ()
header = count 2 $ cell >>= followedBy (many tab >> newline)

-- read titles of column, returns number of columns
title _ = cell `sepBy` tab
        >>= followedBy newline
        >>= return . length

-- read content of a table, ignore worthless footer
content num = fmap concat $ many1 (school num)

-- read a category of school like (기초교육학부)
school num = do
  lss <- many1 (row num) >>= followedBy (try newline >> eof)
  let l = head lss
      scl = head l
      ss = tail lss
   in return $ map ((scl :) . tail) ss
 
-- :: [String]
-- read a row, any data in openlects is consist of a row
row num = do
  x <- count (num-1) (cell >>= followedBy tab)
  y <- cell >>= followedBy newline
  return $ x ++ [y]
  <?> "row"


-- ::[Char]
-- read each cell of a row
cell =
  try (between (char '"') (char '"') (except '"'))
  <|> many (noneOf "\t\n")
  <?> "cell"
