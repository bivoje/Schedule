{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.Char
import Control.Exception
import Control.Monad.Trans.Except
import Control.Monad
import Database.MySQL.Simple
import Text.Parsec.Error
import Parser

-- make ParserError as instance of Exception
-- to use it with other errors in ExceptT monad
-- see insertProfs function
instance Exception ParseError where


tryE :: Exception e => IO a -> ExceptT e IO a
tryE = ExceptT . try


-- prompts user to type in arbitrary number of lines
-- exit prompt by giving empty line
-- returns the collection of lines
promptLinesOf :: (String -> Bool) -> IO [String]
promptLinesOf check = do
  str <- getLine
  proc str
  where proc str | null str  = return []
                 | check str = fmap (str:) $ promptLinesOf check
                 | otherwise = putStrLn "***Error: not aceptable"
                                >> promptLinesOf check


data Teach = PR String
           | TA String
           deriving (Show, Read)


-- check string that might violate the table contents
-- not compatible name fields should be edited manually
isFieldCompatible :: String -> Bool
isFieldCompatible [] = False
isFieldCompatible (s:tr) =
  isLetter s && all (\c -> isLetter c || c==' ') tr


-- loose version of above. (allow leading single space)
-- space starting names are for TAs.
-- used to check user's manual edit
isFieldCompatible' :: String -> Bool
isFieldCompatible' (' ':tr) = isFieldCompatible  tr
isFieldCompatible'     str  = isFieldCompatible str


-- access info to localhost root
mydefaultConnectInfo :: ConnectInfo
mydefaultConnectInfo =
  defaultConnectInfo { connectPassword = "sql"
                     , connectDatabase = "Schedule"
                     }


-- check whether db know the name (field) already
-- 'Query' argument is a kind of trick makin use of
-- (Only) string literals can be intepreted as Query type
-- because variables (QeuryParams) are quoted automatically
-- (e.f user-inputs inevitably be quoted. very clever)
checkNameExist :: Connection -> Query -> String -> IO Bool
checkNameExist conn tbl name = do
  [Only num] <- query conn selq (Only name)
  return $ num /= (0 :: Int)
  where selq = "SELECT COUNT(name) FROM " `mappend` tbl `mappend` " WHERE name = ?"


-- insert name to db if there's no pre-inserted
-- prevent sql-error exceptions
-- FIXME this might be not appropriate if the name field is
-- not primary key anymore
insertNameIfNone :: Connection -> Query -> String -> IO ()
insertNameIfNone conn tbl str = do
  dbknows <- checkNameExist conn tbl str
  when (not dbknows) $
    execute conn exq (Only str)
    >> return ()
  where exq = "INSERT INTO " `mappend` tbl `mappend` "(name) VALUES (?)"


-- merely excutes the insert query to db
exInsertTmpProf :: Connection -> (String,String) -> IO ()
exInsertTmpProf conn n =
  execute conn insertq n >> return ()
  where insertq = "INSERT INTO tmp_professor (name,names) VALUES (?,?)"


-- if name field is not compatible,
-- user will be prompted to type in names manually
-- concats names with leading '|' (professor) or '!' (TA)
refineTmpProf :: String -> IO String
refineTmpProf str =
  if (isFieldCompatible str) then return $ show [PR str]
  else do
    putStrLn ("manual insert required with \"" ++ str ++ "\"")
    names <- promptLinesOf isFieldCompatible'
    return $ show $ map conv names
  where conv (' ':s) = TA s
        conv      s  = PR s


-- FIXME should we handle Exception here?
-- extract professor name from a row, then insert it to db
-- returns the number of inserted rows (of db)
insertTmpProf :: Connection -> [String] -> IO ()
insertTmpProf conn strs = do
  name <- return $ strs !! 5
  dbknows <- (checkNameExist conn "tmp_professor" name)
  when (not dbknows) $ do rfname <- refineTmpProf name
                          exInsertTmpProf conn (name,rfname)


-- insert Teach data to db according to it's position
exInsertTeach :: Connection -> Teach -> IO ()
exInsertTeach conn teach = (case teach of
  (PR s) -> insertNameIfNone conn "professor" s
  (TA s) -> insertNameIfNone conn "ta" s
  ) >> return ()


-- assume tmp_proff exist, move the data from - to prof, ta db
mvTmpToProfs :: Connection -> IO Int
mvTmpToProfs conn = do
  snamess <- query_ conn selq
  let names = concat (map (read.fromOnly) snamess :: [[Teach]])
   in mapM_ (exInsertTeach conn) names >> return (length names)
  where selq = "SELECT names FROM tmp_professor"


-- insert names of professors (and TAs!) to db,
-- also leaves name conversion cache table (tmp_professor)
-- for use of another (course, section ..) parsing
insertProfs :: Connection -> [[String]] -> IO Int
insertProfs conn tbl = do
  execute_ conn createq
  mapM (insertTmpProf conn) (drop 2 tbl)
  mvTmpToProfs conn
  where createq = "\
    \ CREATE TEMPORARY TABLE tmp_professor ( \
    \ name NVARCHAR(50) NOT NULL PRIMARY KEY, \
    \ names NVARCHAR(200) NOT NULL \
    \ );"


-- read openlects and insert professor info of each row to database
-- FIXME still no understands why :: Exception e => ExceptT e IO ()
-- can't work here
run :: ExceptT ParseError IO ()
run = do
  tryE $ hSetBuffering stdin LineBuffering
  conn <- tryE $ connect mydefaultConnectInfo
  tbl <- ExceptT $ parse_openlects "ex_openlects"
  num <- tryE $ insertProfs conn tbl
  tryE (close conn)
  tryE $ print num
