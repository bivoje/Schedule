
module Prompt where

--import Data.Either
import Data.List (findIndex)

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
-- pstr is for for folloing prompt after failing to match within candidatets
-- the first prompt should be printed before this function
-- returns index of user's answer
getAnsWithin :: String -> [String] -> IO Int
getAnsWithin pstr cand =
  getAnsWith f
  where f :: String -> Either String Int
        f s = case findIndex (==s) cand of
                Just i -> Right i
                _      -> Left pstr


-- get user answer in y/n
-- pstr is for first & following prompt
askYesNo :: String -> IO Bool
askYesNo pstr =
  let cand = ["y","n","Y","N","yes","no","Yes","No","YES","NO"]
      pstr' = "please answer in yes/no\n" ++ pstr
   in do putStr pstr
         ansi <- getAnsWithin pstr' cand
         return $ even ansi


-- get str for each field
getStrFields :: [String] -> IO [Maybe String]
getStrFields strs =
  getFieldsWith (Right . nullize) strs
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
                 | check str = fmap (str:) $ promptLinesOf check
                 | otherwise = putStrLn "***Error: not aceptable"
                                >> promptLinesOf check
