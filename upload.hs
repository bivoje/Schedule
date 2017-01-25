{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Control.Exception
import Control.Monad.Trans.Except
import Database.MySQL.Simple
import Text.Parsec.Error
import Parser

-- make ParserError as instance of Exception
-- to use it with other errors in ExceptT monad
-- see insertProfs function
instance Exception ParseError where


tryE :: Exception e => IO a -> ExceptT e IO a
tryE = ExceptT . try

-- some characters that might violate the table contents
-- name fields containing these characters should be handled manually
compatibleChar :: Char -> Bool
compatibleChar c = all (/=c) ("\n|\t" :: [Char])
                && (not $ isControl c)

-- access info to localhost root
mydefaultConnectInfo :: ConnectInfo
mydefaultConnectInfo =
  defaultConnectInfo { connectPassword = "sql"
                     , connectDatabase = "Schedule"
                     }


-- actually excutes the insert query to db
-- returns 0 or 1, the number of row inserted
exInsertProf :: Connection -> String -> IO Int
exInsertProf conn name =
  handle f $ execute conn insertq (Only name) >> return 1
  -- duplicated insert exception will be caught
  where
    insertq = "INSERT INTO professor (name) VALUES (?)"
    f :: SomeException -> IO Int
    f = const (return 0)

-- if name field contains violation characters, the user 
-- will be prompted to insert the names by manual
-- returns the number of rows inserted
promptName conn =
  getLine >>= (\name -> if null name then return 0 else prmt name)
  where
    prmt str = do
      n <- exInsertProf conn str
      m <- promptName conn
      return $ n + m

-- extract professor name from a row, then insert it to db
-- returns the number of inserted rows (of db)
insertProf :: Connection -> [String] -> IO Int
insertProf conn strs =
  --let name = takeWhile compatibleChar $ strs !! 5
  let name = strs !! 5
   in if all compatibleChar name
      then exInsertProf conn name
      else putStrLn ("manual insert required with \"" ++ name)
           >> promptName conn

-- read openlects and insert professor info of each row to database
-- FIXME still no understands why :: Exception e => ExceptT e IO ()
-- can't work here
insertProfs :: ExceptT ParseError IO ()
insertProfs = do
  tbl <- ExceptT $ parse_openlects "ex_openlects"
  conn <- tryE (connect mydefaultConnectInfo)
  exts <- tryE (mapM (insertProf conn) (drop 2 tbl))
  tryE (close conn)
  tryE $ print $ sum exts
