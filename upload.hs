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
isFieldCompatible' (' ':tr) = isFieldCompatible tr
isFieldCompatible' str = isFieldCompatible str


-- access info to localhost root
mydefaultConnectInfo :: ConnectInfo
mydefaultConnectInfo =
  defaultConnectInfo { connectPassword = "sql"
                     , connectDatabase = "Schedule"
                     }


-- merely excutes the insert query to db
exInsertTmpProf :: Connection -> (String,String) -> IO ()
exInsertTmpProf conn n =
  execute conn insertq n >> return ()
  where insertq = "INSERT INTO tmp_professor (name,names) VALUES (?,?)"


-- check whether db know the name already
checkTmpProfExist :: Connection -> String -> IO Bool
checkTmpProfExist conn name = do
  [Only num] <- query conn selq (Only name)
  return $ num /= (0 :: Int)
  where selq = "SELECT COUNT(name) FROM tmp_professor WHERE name = ?"


-- if name field is not compatible,
-- user will be prompted to type in names manually
-- concats names with leading '|' (professor) or '!' (TA)
refineTmpProf :: String -> IO String
refineTmpProf str =
  if (isFieldCompatible str) then return ('|':str)
  else do
    putStrLn ("manual insert required with \"" ++ str ++ "\"")
    names <- promptLinesOf isFieldCompatible'
    return $ foldr fusion [] names
  where fusion (' ':s) acc = '!' : s ++ acc
        fusion      s  acc = '|' : s ++ acc


-- extract professor name from a row, then insert it to db
-- returns the number of inserted rows (of db)
insertTmpProf :: Connection -> [String] -> IO ()
insertTmpProf conn strs = do
   name <- return $ strs !! 5
   rfname <- refineTmpProf name
   dbknows <- (checkTmpProfExist conn name)
   when (not dbknows) $ exInsertTmpProf conn (name,rfname)
