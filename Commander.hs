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
taskHandle' :: (String -> IO a -> IO (Maybe a))
            -> String -> IO a -> IO (Maybe a)
taskHandle' reta tstr action =
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
      reta tstr action


-- ask user y/n and do the task if anser is yes
retask :: String -> IO a -> IO (Maybe a)
retask tstr action = do
  b <- askYesNo
  if b then taskHandle' retask tstr action
       else return Nothing


-- enboxes given io action with task (user interpreting handler)
-- if unhandled exception occurs, user will asked to select
-- between retry or halting
-- returns Nothing if the action is halted
task :: String -> IO a -> MaybeT IO a
task tstr action = MaybeT $ do
  putStrLn $ "task: " ++ tstr
  retask tstr action


-- TODO lower case the input? -> case insesitive
-- does same work as retask, with appeded 'skip' option
retask_ :: String -> IO a -> IO (Maybe ())
retask_ tstr action = do
  i <- getAnsWithin "yes/no/skip" [
         "y","n","s",
         "Y","N","S",
         "yes","no","skip",
         "Yes","No","Skip",
         "YES","NO","SKIP"]
  case mod i 3 of
    0 -> taskHandle' retask_ tstr (action >> return ())
    1 -> return Nothing
    2 -> return (Just ())

-- this function throw away the result of action.
-- just like mapM_ does
task_ :: String -> IO a -> MaybeT IO ()
task_ tstr action = MaybeT $ do
  putStrLn $ "task: " ++ tstr
  retask_ tstr action


-- enboxes a parsing action (io $ parse + parser + stream)
-- to compatible with task function
-- throws unhandled exception 'ParseError' when parsing fails
parsingTask :: IO (Either P.ParseError a) -> IO a
parsingTask pa = do
  ret <- pa
  case ret of Left e -> throw e
              Right a -> return a


insertTask :: IO Int -> IO ()
insertTask action = do
  n <- action
  putStrLn $ "inserted " ++ show n ++ " elements"


-- run necessary tasks for uploading in sequence
runTask conn = runMaybeT $ do
  obl <- task "loading openlects" $ parsingTask (parse_openlects "ex_openlects")
  tbl <- task "loading timetable" $ parsingTask (parse_timetable "ex_timetable")
  task_ "inserting professors" $ insertTask (insertProfs conn obl)
  task_ "inserting courses" $ insertTask (insertCourses conn obl)
  task_ "inserting sections" $ insertTask (insertSects conn obl)
  -- wee need to edit insertRooms/Tmts 's return
  task_ "inserting rooms" $ insertTask (insertRooms conn tbl >> return 0)
  task_ "inserting timetable" $ insertTask (insertTmts conn tbl >> return 0)



----------------------------------------------------------------------
-- main


-- ask user for information then connect to db server
connectPrompt :: IO Connection
connectPrompt = do
  putStrLn "enter connection information"
  u <- putStr "user: " >> getLine
  h <- putStr "host: " >> getLine
  p <- getNumber "port: "
  d <- putStr "dtbs: " >> getLine
  pws <- putStr "passwd: " >> getLine
  putStrLn "" -- stdin's newline (enter) not echoed
  putStrLn "connecting to db.."
  connect defaultConnectInfo
    { connectHost = h
    , connectPort = fromInteger $ toInteger p
    , connectUser = u
    , connectDatabase = d
    , connectPassword = pws
    }


-- main functions
-- connects to server, does the tasks,
-- closes connection even though exception occurs
main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  bracket (connectPrompt) close runTask
    `catch` (\(e::MySQLError) -> do
       putStrLn "check your connection to db"
       putStr $ indentError "  " e
       return Nothing
     )
  return ()
