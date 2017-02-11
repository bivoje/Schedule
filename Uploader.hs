{-# LANGUAGE OverloadedStrings #-}

module Uploader
  ( insertProfs
  , insertCourses
  , insertSects
  , insertRooms
  , insertTmts
  )
  where

import Data.List (transpose, groupBy)
import Data.Char
import Data.Function (on)
import Data.Maybe
import Control.Monad (when,guard)
import Control.Exception
import System.IO.Error
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Base (MySQLError(..))

import Parser (parse_requir)
import Prompt


nullize :: String -> Maybe String
nullize s = guard (not $ null s) >> return s


-- span, omits the separator
omitSpan :: (a -> Bool) -> [a] -> ([a],[a])
omitSpan f ls = case span f ls of (as,c:bs) -> (as,bs)
                                  x         -> x


boolMaybe :: (a -> Bool) -> a -> Maybe a
boolMaybe f a = guard (f a) >> return a


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = reverse . takeWhile f . reverse


data Teach = PR String
           | TA String
           deriving (Show, Read)


-- excute given (probably inserting query) io action
-- if MySQLError occurs, handle it
exInsertHandler :: (QueryParams q, Show q, Read q)
                => (q -> IO Bool) -> q -> IO Bool
exInsertHandler act q =
  catchJust f (act q) (handleInsert act q)
  where f e = if errNumber e `elem` [1062, 1452]
              then Just (errNumber e) else Nothing


-- handle MySQLError exception
handleInsert :: (QueryParams q, Show q, Read q) => (q -> IO Bool) -> q -> Int -> IO Bool
handleInsert act q eno = do
  putStrLn $ case eno of
    1062 -> "dupkey error occured while inserting " ++ show q
    1452 -> "foreign key constraints failed inserting " ++ show q
  putStr "retry/skip/input/halt : "
  ans <- getAnsWithin "please answer in (r/s/i/h): " ["r", "s", "i", "h"]
  case ans of 0 -> exInsertHandler act q
              1 -> return False
              2 -> readq >>= exInsertHandler act
              3 -> throw $ userError "halt during inserting tmp prof"
  where readq = putStrLn "arg: " >> getAnsWith (\s -> case reads s of
          [(a,"")] -> Right a
          [(a,t)] -> Left ("Trailing characters \"" ++ t ++ "\"")
          [] -> Left "could not parse!! re-enter\n arg: ")

-- check whether db know the name (field) already
-- 'Query' argument is a kind of trick makin use of
-- (Only) string literals can be intepreted as Query type
-- because variables (QeuryParams) are quoted automatically
-- (e.f user-inputs inevitably be quoted. very clever)
checkKeyInDB :: QueryParams q
             => Connection -> Query -> Query -> q -> IO Bool
checkKeyInDB conn tbl key val = do
  [Only num] <- query conn selq val
  return $ num /= (0 :: Int)
  where selq = mconcat ["SELECT COUNT(", key, ") FROM ", tbl, " WHERE ", key, "= ?"]



----------------------------------------------------------------------
-- insert Professor



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


-- get teacher names manually
getManualTeaches :: IO [Teach]
getManualTeaches = do
    names <- promptLinesOf isFieldCompatible'
    return . catMaybes . map (fmap conv . nullize) $ names
  where conv (' ':s) = TA s
        conv      s  = PR s

getManualTeachPair :: IO (Maybe String, Maybe String)
getManualTeachPair = do
  pstr <- getFieldsWith f ["pr", "ta"]
  return (pstr!!0, pstr!!1)
  where f s = if null s || isFieldCompatible s
              then Right (nullize s)
              else Left "***Error: not aceptable"

-- if name field is not compatible,
-- user will be prompted to type in names manually
-- concats names with leading '|' (professor) or '!' (TA)
refineTmpProf :: String -> IO String
refineTmpProf str =
  if (isFieldCompatible str) then return $ show [PR str]
  else do
    putStrLn ("manual insert required with \"" ++ str ++ "\"")
    fmap show getManualTeaches


-- merely excutes the insert query to db with error handling
exInsertTmpProf :: Connection -> (String,String) -> IO Bool
exInsertTmpProf conn q =
  exInsertHandler (\n -> execute conn insq n >> return True) q
  where
    insq = "INSERT INTO che_professor (name,names) VALUES (?,?)"


-- extract professor name from a row, then insert it to db
-- returns the number of inserted rows (of db)
insertTmpProf :: Connection -> String -> IO Bool
insertTmpProf conn name = do
  dbknows <- (checkKeyInDB conn "che_professor" "name" (Only name))
  if dbknows then return False else do
    rfname <- refineTmpProf name
    exInsertTmpProf conn (name,rfname)


-- insert Teach data to db according to it's position (prof/ta ..)
exInsertTeach :: Connection -> Teach -> IO Bool
exInsertTeach conn teach = case teach of
  (PR s) -> insertNameIfNone "professor" s
  (TA s) -> insertNameIfNone "ta" s
  where
    insertNameIfNone tbl str =
      let insq = mconcat ["INSERT INTO ", tbl, "(name) VALUES (?)"]
          p = (Only str)
       in exInsertHandler (\q -> execute conn insq q >> return True) p


-- move the data from - to prof, ta db
mvTmpToProfs :: Connection -> IO Int
mvTmpToProfs conn = do
  -- FIXME should i handle this?
  snamess <- query_ conn selq
  names <- return $ concatMap (read.fromOnly) snamess :: IO [Teach]
  bs <- mapM (exInsertTeach conn) names
  return . length $ filter id bs
  where selq = "SELECT names FROM che_professor"


-- top level of inserting to table 'professor'
-- insert names of professors (and TAs!) to db,
-- also fills up cache table name conversion (che_professor)
-- for use of another (course, section ..) task
insertProfs :: Connection -> [[String]] -> IO Int
insertProfs conn tbl = do
  bs <- mapM (insertTmpProf conn) $ transpose (drop 2 tbl) !! 5
  putStrLn "che_prof filled, start moving to prof"
  mvTmpToProfs conn
  return . length . filter id $ bs



----------------------------------------------------------------------
-- insert Course



-- get the sect number from the title field
readsCredit :: String -> [(Int,String)]
readsCredit =
  reads . takeWhileEnd isNumber


-- gets the strings to insert db from parsed one
refineTlts :: String -> (String,String)
refineTlts tlt' = let (tkr,tlt) = omitSpan (/='\n') tlt'
                   in (strip . map sub $ tlt, strip tkr)
  where
    lstrip = dropWhile isSpace
    strip = lstrip . reverse . lstrip . reverse
    sub c | isAscii c = c
          | otherwise =
          -- FIXME not error, but user input
            let err = error $ '"' : (show c) ++ "\""
             in fromMaybe err $ lookup c [('Ⅱ','2'),('Ⅰ','1')]


-- merely excutes the insert query to db
exInsertCourse :: Connection
               -> (String,String,String,Int
                  ,Int,Maybe String,Maybe String,Maybe String)
               -> IO Bool
exInsertCourse conn args =
  exInsertHandler (\q -> execute conn insq q >> return True) args
  where insq = "\
    \ INSERT INTO \
    \   course (crs_id, title, title_kr, credit, \
    \   semester, requir1, requir2, requir3) \
    \ VALUES (?,?,?,?,?,?,?,?) \
    \ ;"


-- insert single course, with given (unchecked) tuple data of strings
insertCourse :: Connection
             -> (String,String,String,String,String)
             -> IO Bool
-- school (sch) value is aready in crs, we don't need it
-- also, we don't utilize classify (cls) yet
insertCourse conn (crs,_,tlt',cre',req') =
  let (tlt,tkr) = refineTlts tlt'
      -- TODO this should be able to set manually
      sme = 216
  in do
    cre <- getcred
    (rq1,rq2,rq3) <- getreqs >>= (\ls -> return (ls!!0,ls!!1,ls!!2))
    exInsertCourse conn (crs,tlt,tkr,cre,sme,rq1,rq2,rq3)
  where
    getcred = case readsCredit cre' of
      [(c,_)] -> return c
      [] -> do
        putStrLn "could not parse credit from.."
        putStrLn $ "\"" ++ cre' ++ "\""
        putStr "please enter manually\ncre: "
        getAnsWith (\s -> if all isNumber s
                          then Right $ (read s :: Int)
                          else Left "please enter a number\ncre: ")
    getreqs = case parse_requir req' of
      Right ls -> return . map nullize $ ls ++ ["","",""]
      Left _ -> do
        putStrLn "could not parse requir list from.."
        putStrLn $ "\"" ++ req' ++ "\""
        putStrLn "please enter manually"
        getStrFields ["rq1", "rq2", "rq3"]


-- top level of inserting to table 'course'
-- does not depend on che_professor
-- returns the number of courses inserted
insertCourses :: Connection -> [[String]] -> IO Int
insertCourses conn tbl =
  -- (crs_id, classify, title', credit', requir')
  let contbl = drop 2 tbl
      nubtbl = map head $ groupBy ((==) `on` (!!3)) contbl
      tups = map (\s -> (s!!2,s!!1,s!!3,s!!4,s!!9)) nubtbl
      num = length tups
   in fmap (length . filter id) $ mapM (insertCourse conn) tups



----------------------------------------------------------------------
-- insert Section


{-
instance Eq Teach where
  (PR x) == (PR y) = x == y
  (TA x) == (TA y) = x == y
  _ == _ = False

instance Ord Teach where
  (PR x) <= (PR y) = x <= y
  (TA x) <= (TA y) = x <= y
  (PR _) <= (TA _) = False
  (TA _) <= (PR _) = True
-}

getCacheProf :: Connection -> String -> IO [Teach]
getCacheProf conn str = do
  sel <- query conn selq (Only str)
  case sel of
    (Only x):_ -> return.read $ x
    [] -> do
      putStrLn $ "can't find cache for \"" ++ str ++ "\""
      putStrLn "please insert the cache data manually"
      prta <- getManualTeaches
      -- FIXME these are not handled by insertHandler!!
      exInsertTmpProf conn (str, show prta)
      mapM_ (exInsertTeach conn) prta
      return prta
  where
    selq = "SELECT names FROM che_professor WHERE name = ?"


getProfFromTmp :: Connection -> String -> IO (Maybe String, Maybe String)
getProfFromTmp conn str = do
  ts <- getCacheProf conn str
  case ts of []             -> return (Nothing, Nothing)
             [PR pr]        -> return (Just pr, Nothing)
             [PR pr, TA ta] -> return (Just pr, Just ta)
             [TA ta, PR pr] -> return (Just pr, Just ta)
             [TA _] -> putStrLn "only ta?? you better update cache_prof"
                       >> promptTeachers ts
             _ -> promptTeachers ts
  where
    promptTeachers ts = do
      putStrLn "could not get pr/ta properly from cache of.."
      putStrLn $ "\"" ++ str ++ "\""
      putStrLn "please enter manually"
      getManualTeachPair


insertSect :: Connection -> Int -> (String,String,String,String)
             -> IO Bool
-- we can check secn (int) with sectno (string) but we don't..
insertSect conn secn (_,crsid,prof',enrol') =
  let enrol = fmap fst . listToMaybe $ (reads enrol' :: [(Int,String)])
  in do
    (prof,ta) <- getProfFromTmp conn prof'
    args <- return (secn,crsid,prof,ta,enrol)
    exInsertHandler (\q -> execute conn insq q >> return True) args
  where insq = "\
    \ INSERT INTO \
    \   section (sect_no, crsid, prof, ta, enroll_size) \
    \ VALUES (?,?,?,?,?); \
    \ "


insertSectGrp :: Connection -> [(String,String,String,String)] -> IO Int
insertSectGrp conn grps =
  let ex = mapM (uncurry $ insertSect conn) $ zip [1..] grps
   in fmap (length . filter id) ex


insertSects :: Connection -> [[String]] -> IO Int
insertSects conn tbl =
  -- (sect#, crsid, prof(ta), enroll)
  let contbl = drop 2 tbl
      tups = map (\s -> (s!!7,s!!2,s!!5,s!!8)) contbl
      gtups = groupBy ((==) `on` snd4) tups
   in fmap sum $ mapM (insertSectGrp conn) gtups
  where snd4 (_,b,_,_) = b



----------------------------------------------------------------------
-- insert Room



-- datatype used to deal with parsed data of timetable
-- vertical block of cell
type Vcell = (String,String,String,String)
-- Vcells in single period of a day,
-- table[time_period][day]
type Vcells = [Vcell]
-- Vcells in a period of many day
type Pcell = [Vcells]
-- all the Vcells
type Pcells = [Pcell]
-- Vcells in a day
type Dcell = [Vcells]
-- all the Vcells
type Dcells = [Dcell]


-- filters out empty Vcell from table
-- doesn't matter whether Pcells / Dcells
filterEmpty :: [[Vcells]] -> [[Vcells]]
filterEmpty = map (map (filter filterf))
  where filterf (a,_,_,_) = not $ null a


-- get the sect number from the title field
getSectno :: String -> Int
getSectno str =
  case dropWhile (/='(') str of
    '(':xs -> read $ takeWhile isNumber xs
    [] -> 1


-- merely excutes the query to db
-- (room, crsid, sectno)
exInsertRoom :: Connection -> (String,String,Int) -> IO ()
exInsertRoom conn tup =
  print tup >>
  execute conn updq tup >> return ()
  where updq = "\
    \ UPDATE section \
    \ SET room = ? \
    \ WHERE crsid = ? AND sect_no = ? \
    \;"

-- update table 'section' with room the infomation
insertRooms :: Connection -> Pcells -> IO ()
insertRooms conn pcells =
  let vcells = filterNoRoom . concat . concat $ pcells
   in mapM_ (exInsertRoom conn . g) vcells
  where filterNoRoom = filter f
        f (_,_,_,d) = not $ null d
        g (a,b,c,d) = (d,a,getSectno(c))



----------------------------------------------------------------------
-- insert TimeTable



exInsertTmt :: Connection -> (String,Int,String, Int) -> IO ()
exInsertTmt conn arg =
  print arg >>
  execute conn insq arg >> return ()
  where insq = "\
    \ INSERT INTO \
    \   class (crsid,sectno,day,period) \
    \ VALUES (?,?,?,?) \
    \;"


insertVcells :: Connection -> Int -> Int -> Vcells -> IO ()
insertVcells conn np nd vcells =
  let days = ["MON", "TUE", "WED", "THR", "FRI", "SAT", "SUN"]
      d = days !! (nd-1)
      p = np
   in mapM_ (exInsertTmt conn . insvc p d) vcells
  where insvc p d (a,_,c,_) = (a,getSectno(c),d,p)


insertTmts :: Connection -> Pcells -> IO ()
insertTmts conn pcells =
  mapM_ (uncurry insp) $ zip [1..] (filterEmpty pcells)
  where insp p pcell =
          mapM_ (uncurry $ insertVcells conn p) $ zip [1..] pcell
