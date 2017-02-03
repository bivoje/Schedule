{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Parser (parse_openlects,parse_timetable,word) where

import Data.Char (isSpace)
import Data.List
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
parse_openlects file = readFile file >>= return . parse opl_table file

-- read whole table, returns its content
opl_table = do
  hd <- opl_header
  tl <- opl_title
  ct <- opl_content (length tl)
  return $ hd : tl : ct

-- read heading of table (name of table etc..) returns ()
opl_header = count 2 line >> return []

-- read titles of column, returns number of columns
opl_title = cells

-- read content of a table, ignore worthless footer
opl_content num = fmap concat $ many1 (school num)

-- read a category of school like (기초교육학부)
school num = do
  lss <- many1 (opl_row num) >>= followedBy (try newline >> eof)
  let l = head lss
      scl = head l
      ss = tail lss
   in return $ map ((scl :) . tail) ss
 
-- :: [String]
-- read a row, any data in openlects is consist of a row
opl_row num = do
  x <- count (num-1) (cell >>= followedBy tab)
  y <- cell >>= followedBy newline
  return $ x ++ [y]
 <?> "opl_row"


-------------------------------------------------------------------
-- Timetable


-- top-most utility function
parse_timetable file = readFile file >>= return . parse tmt_table file

-- read whole table, returns its content
tmt_table = do
  tl <- tmt_title
  ct <- tmt_content (tail $ map snd tl)
  return ct

-- read title of each column,
-- used to calculate how many cell are in each day
tmt_title = many1 mergedCell >>= followedBy newline
            <?> "title"

-- read content of table
tmt_content ncells = do
  morning    <- count 2 tp
  line --lunch time
  afternoon  <- count 5 tp
  many emptyline
  experiment <- count 2 tp
  many emptyline
  activity   <- count 3 tp
  return (morning ++ afternoon ++ experiment ++ activity)
 <?> "content"
  where tp = timep ncells

-- read single time period
-- cell of beginig is for timep name e.g. "1st Class (09:00-10:15)"
timep ncells = do
  cell
  cs <- many1 tmt_row
  return $ daize cs ncells
 <?> "timep"
  where daize _ [] = []
        daize rows (n:ncs) =
          let sp = map (splitAt n) rows
           in concatMap fst sp : daize (map snd sp) ncs

-- read a row of vertical blocks consists of
-- [Crs. No., Instructor, Class, Title Room]
-- then remove labels, group the block by each
tmt_row = do
  a <- try $ count 4 (tab >> cells)
  return . map tuplize . tail . transpose $ a
 <?> "tmt_row"
  where tuplize [a,b,c,d] = (a,b,c,d)


-------------------------------------------------------------------
-- Primary


-- read a horizontally merged cell
-- returns the number of cells merged with the cell content
mergedCell = do
  c <- cell
  t <- many1 tab
  return (c, length t)
 <?> "mergedCell"

-- returns a line of cells
cells = cell `sepBy` tab >>= followedBy newline
        <?> "cells"

-- ::[Char]
-- read each cell of a row
cell =
  -- FIXME this can't handle escaped " (\") in the string
  try (between (char '"') (char '"') (except '"'))
  <|> many (noneOf "\t\n")
  <?> "cell"


-- skip an empty line (filled with tabs)
-- quoted \n character not supported
emptyline = skipMany tab >> newline >> return ()
            <?> "emptyline"

-- skips a line
line = except '\n' >>= followedBy newline
       <?> "line"

followedBy :: Stream s m Char
           => ParsecT s u m t -> a -> ParsecT s u m a
followedBy p a = p >> return a

word = between spaces spaces $ many1 (satisfy $ not.isSpace)

except :: Stream s m Char => Char -> ParsecT s u m [Char]
except c = many $ satisfy (/=c)
