{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

import System.IO
import System.IO.Error (isDoesNotExistError)

import Control.Exception
import Control.Monad.Trans.Maybe
import Database.MySQL.Simple
import Database.MySQL.Base (MySQLError)
import Data.List

import Parser as P
import Uploader
import Prompt


instance Exception ParseError


-- format error messege by indenting with given token
indentError :: Exception e => String -> e -> String
indentError ind e = unlines . map (ind ++ ) . lines $ show e



----------------------------------------------------------------------
-- task



-- catches unhandled exceptions withing the task (action
-- unhandled exception:
--   those circumstances that continueing is undesirable
--   recovery possible only if we restart the task
taskHandle :: String -> IO a -> IO (Maybe a)
taskHandle tstr action =
  catchJust f (fmap Just action) h `catches`
    [ Handler (\ (e :: MySQLError ) -> h e)
    , Handler (\ (e :: FormatError) -> h e)
    , Handler (\ (e :: QueryError ) -> h e)
    , Handler (\ (e :: ResultError) -> h e)
    -- FIXME isn't this too inefficient?
    , Handler (\ (e :: ParseError ) -> h e)
    ]
  where
    f e = if isDoesNotExistError e then Just e else Nothing
    h e = do
      putStrLn "Unhandled Exception occured"
      putStr $ indentError "  " e
      putStrLn $ "Do you want to run the task (" ++ tstr ++ ") again?"
      retask tstr action


-- ask user y/n and do the task if anser is yes
retask :: String -> IO a -> IO (Maybe a)
retask tstr action = do
  b <- askYesNo "Yes/No: "
  if b then taskHandle tstr action  else return Nothing


-- enboxes given io action with task (user interpreting handler)
-- if unhandled exception occurs, user will asked to select
-- between retry or halting
-- returns Nothing if the action is halted
task :: String -> IO a -> MaybeT IO a
task tstr action = MaybeT $ do
  putStrLn $ "task: " ++ tstr
  retask tstr action


-- enboxes a parsing action (io $ parse + parser + stream)
-- to compatible with task function
-- throws unhandled exception 'ParseError' when parsing fails
parsingTask :: String -> IO (Either P.ParseError a) -> MaybeT IO a
parsingTask tstr pa = task tstr $ do
  ret <- pa
  case ret of Left e -> throw e
              Right a -> return a


-- run necessary tasks for uploading in sequence
runTask conn = runMaybeT $ do
  otbl <- parsingTask "loading openlects" (parse_openlects "ex_openlects")
  ttbl <- parsingTask "loading timetable" (parse_timetable "ex_timetable")
  --task "inserting Professor" (insertProf otbl)

  return ()


----------------------------------------------------------------------
-- main



-- access info to database
-- FIXME currently, information of test database 'Tchedule'
mydefaultConnectInfo :: ConnectInfo
mydefaultConnectInfo =
  defaultConnectInfo { connectHost = "192.168.0.4"
                     , connectUser = "scheduleM"
                     , connectDatabase = "Tchedule"
                     }


-- ask user for passwd to db, then connect to it
-- FIXME getting passwd using getLine reveals passwd to screan
connectWithPasswd :: IO Connection
connectWithPasswd = do
  putStrLn "connecting to db.."
  putStr "passwd: "
  psw <- getLine
  connect mydefaultConnectInfo {connectPassword=psw}


-- main functions
-- connects to server, does the tasks,
-- closes connection even though exception occurs
main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  bracket (connectWithPasswd) close runTask
    `catch` (\(e::MySQLError) -> do
       putStrLn "check your connection to db"
       putStr $ indentError "  " e
       return Nothing
     )
  return ()
