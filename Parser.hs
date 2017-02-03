{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (parse_openlects,word) where

import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Token


-------------------------------------------------------------------
-- Openlects


-- NOTICE
-- some modification required before copying!
-- 1. there should be no double quote charactor (") within excel file
--    erase them or replace it with another (like (``))
--    additionally, I did erased meaningless column
--    (probably being there for padding) for ease of parsing
--    TODO we may use getContents instead, for memory efficiency
-- 2. a file assumed to have only one school category.
--    it can be extended in case of need


-- FIXME readFile might produce an exception
parse_openlects file = readFile file >>= return . parse table file

-- read whole table, returns its content
table = do
  hd <- header
  tl <- title
  ct <- content (length tl)
  return $ hd : tl : ct

-- read heading of table (name of table etc..) returns ()
header = count 2 $ cell >>= followedBy (many tab >> newline) >> return []

-- read titles of column, returns number of columns
title = cell `sepBy` tab
        >>= followedBy newline

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


-------------------------------------------------------------------
-- Primary


-- ::[Char]
-- read each cell of a row
cell =
  -- FIXME this can't handle escaped " (\") in the string
  try (between (char '"') (char '"') (except '"'))
  <|> many (noneOf "\t\n")
  <?> "cell"

followedBy :: Stream s m Char
           => ParsecT s u m t -> a -> ParsecT s u m a
followedBy p a = p >> return a

word = between spaces spaces $ many1 (satisfy $ not.isSpace)

except :: Stream s m Char => Char -> ParsecT s u m [Char]
except c = many $ satisfy (/=c)
