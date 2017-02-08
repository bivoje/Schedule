{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

import System.IO

import Control.Exception
import Control.Monad.Trans.Maybe
import Database.MySQL.Simple
import Database.MySQL.Base (MySQLError)
import Data.List

import Parser as P
import Uploader

instance Exception ParseError

indentError :: Exception e => String -> e -> String
indentError ind e = unlines . map (ind ++ ) . lines $ show e

----------------------------------------------------------------------
-- get input


getAnsWith :: (String -> Either String a) -> IO a
getAnsWith pred = do
  ans <- getLine
  case pred ans of
    Left errstr -> putStr errstr >> getAnsWith pred
    Right a -> return a

getAnsWithin :: String -> [String] -> IO Int
getAnsWithin pstr ss =
  getAnsWith f
  where f :: String -> Either String Int
        f s = case findIndex (==s) ss of
                Just i -> Right i
                _      -> Left pstr

askYesNo :: String -> IO Bool
askYesNo pstr =
  let cand = ["y","n","Y","N","yes","no","Yes","No","YES","NO"]
      pstr' = "please answer in yes/no\n" ++ pstr
   in do putStr pstr
         ansi <- getAnsWithin pstr' cand
         return $ even ansi


----------------------------------------------------------------------
-- task


-- catches tasks unhandled within the action
-- unhandled tasks occurs when continueing task is undesirable
-- recovery possible only if restarting job.
-- exceptions can be handled within the anction if recovering
-- is possible
taskHandle :: String -> IO a -> IO (Maybe a)
taskHandle tstr action =
  fmap Just action `catches`
    [ Handler (\ (e :: MySQLError ) -> h e)
    , Handler (\ (e :: FormatError) -> h e)
    , Handler (\ (e :: QueryError ) -> h e)
    , Handler (\ (e :: ResultError) -> h e)
    -- isn't this too inefficient?
    , Handler (\ (e :: ParseError ) -> h e)
    ]
  where
    h e = do
      putStrLn "Unhandled Exception occured"
      putStr $ indentError "  " e
      putStrLn $ "Do you want to run the task (" ++ tstr ++ ") again?"
      retask tstr action

retask :: String -> IO a -> IO (Maybe a)
retask tstr action = do
  b <- askYesNo "Yes/No: "
  if b then taskHandle tstr action  else return Nothing

task :: String -> IO a -> MaybeT IO a
task tstr action = MaybeT $ do
  putStrLn $ "task: " ++ tstr
  retask tstr action

runTask :: Connection -> IO (Maybe ())
runTask conn = runMaybeT $ do
  -- something to do
  return ()

----------------------------------------------------------------------
-- main

-- access info to localhost root
mydefaultConnectInfo :: ConnectInfo
mydefaultConnectInfo =
  defaultConnectInfo { connectHost = "192.168.0.4"
                     , connectUser = "scheduleM"
                     , connectDatabase = "Tchedule"
                     }

connectWithPasswd :: IO Connection
connectWithPasswd = do
  putStrLn "connecting to db.."
  putStr "passwd: "
  psw <- getLine
  connect mydefaultConnectInfo {connectPassword=psw}

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


{-
 - (MySQL)
 - MySQLError
 -   |ConnectionError
 -   |ResultError
 - FormatError
 - QueryError
 - ResultError
 -}
