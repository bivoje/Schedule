
module Prompt where

--import Data.Either
import Data.List (elemIndex)
import Data.Char (isNumber)

-- get user input satisfies given predicate
-- repeat asking when pred fails.
-- the first prompt should be printed before this function
-- when pred result to left, result string will be used as next prompt
-- otherwise, returns result processed by predicate
getAnsWith :: (String -> Either String a) -> IO a
getAnsWith pred = do
  ans <- getLine
  case pred ans of
    Left errstr -> putStr errstr >> getAnsWith pred
    Right a -> return a


-- get user input within given candidates
-- str is candidate string like "yes/no"
-- it generates the first and following prompt itself
-- returns index of user's answer
getAnsWithin :: String -> [String] -> IO Int
getAnsWithin cstr cand =
  putStr (cstr ++ ": ") >> getAnsWith f
  where f :: String -> Either String Int
        f s = case elemIndex s cand of
                Just i -> Right i
                _      -> Left ("please answer within " ++ cstr ++ ": ")


-- get user answer in y/n
-- pstr is for first & following prompt
askYesNo :: IO Bool
askYesNo =
  let cand = ["y","n","Y","N","yes","no","Yes","No","YES","NO"]
   in do ansi <- getAnsWithin "yes/no" cand
         return $ even ansi


-- get str for each field
getStrFields :: [String] -> IO [Maybe String]
getStrFields = getFieldsWith (Right . nullize)
  where nullize s = if null s then Nothing else Just s


getFieldsWith :: (String -> Either String a) -> [String] -> IO [a]
getFieldsWith pred = getFieldsWiths . map (\s -> (s,pred))


-- get str for each field
getFieldsWiths :: [(String, String -> Either String a)] -> IO [a]
getFieldsWiths = mapM getans
  where getans (s,f) = do
          putStr (s ++ ": ")
          x <- getLine
          case f x of
            Left errstr -> putStrLn errstr >> getans (s,f)
            Right a -> return a


-- prompts user to type in arbitrary number of lines
-- exit prompt by giving empty line
-- returns the collection of lines
promptLinesOf :: (String -> Bool) -> IO [String]
promptLinesOf check = do
  str <- getLine
  proc str
  where proc str | null str  = return []
                 | check str = (str:) <$> promptLinesOf check
                 | otherwise = putStrLn "***Error: not aceptable"
                                >> promptLinesOf check

getNumber :: String -> IO Int
getNumber pstr = putStr pstr >> getAnsWith (\s ->
  if not (null s) && all isNumber s
  then Right (read s :: Int)
  else Left $ "please enter a number\n" ++ pstr)


getNumberM :: String -> IO (Maybe Int)
getNumberM pstr = putStr pstr >> getAnsWith f
  where f s | null s = Right Nothing
            | all isNumber s = Right $ Just (read s :: Int)
            | otherwise = Left $ "please enter a number\n" ++ pstr
