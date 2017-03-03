{-# LANGUAGE OverloadedStrings #-}

module Uploader
  ( UploaderUserHalting
  , insertTmpProfs
  , mvTmpToProfs
  , insertCourses
  , insertRequirs
  , insertSects
  , insertRooms
  , insertTmts
  )
  where

import Data.List (transpose, groupBy)
import Data.Char
import Data.Function (on)
import Data.Maybe
import Control.Monad
import Control.Exception
import System.IO.Error
import Database.MySQL.Simple hiding (errMessage)
import Database.MySQL.Simple.QueryParams (QueryParams)
import Database.MySQL.Base (MySQLError(..))

import Parser (parse_requir,parse_credit,parse_crsid)
import Prompt


nullize :: String -> Maybe String
nullize s = guard (not $ null s) >> return s


-- span, omits the separator
omitSpan :: (a -> Bool) -> [a] -> ([a],[a])
omitSpan f ls = case span f ls of (as,c:bs) -> (as,bs)
                                  x         -> x


countTrue :: [Bool] -> Int
countTrue = length . filter id


data Teach = PR String
           | TA String
           deriving (Show, Read)

data UploaderUserHalting = UploaderUserHalting

instance Show UploaderUserHalting where
  show e = "user halted during process"

instance Exception UploaderUserHalting

-- excute given (probably inserting query) io action
-- if MySQLError occurs, handle it
exInsertHandler :: (QueryParams q, Show q, Read q)
                => (q -> IO Bool) -> q -> IO Bool
exInsertHandler act q =
  catchJust f (act q) (handleInsert act q)
  where f e = if errNumber e `elem` [1062, 1452]
              then Just e else Nothing


-- handle MySQLError exception
handleInsert :: (QueryParams q, Show q, Read q)
             => (q -> IO Bool) -> q -> MySQLError -> IO Bool
handleInsert act q e = do
  putStr $ genErrorMessage e q
  ans <- getAnsWithin "retry/skip/input/halt" ["r", "s", "i", "h"]
  case ans of 0 -> exInsertHandler act q
              1 -> return False
              2 -> readq >>= exInsertHandler act
              3 -> throw UploaderUserHalting
  where readq = putStr "arg: " >> getAnsWith (\s -> case reads s of
          [(a,"")] -> Right a
          [(a,t)] -> Left ("Trailing characters \"" ++ t ++ "\"")
          [] -> Left "could not parse!! re-enter\narg: ")


genErrorMessage :: (QueryParams q, Show q) => MySQLError -> q -> String
genErrorMessage e q = unlines
  [ case errNumber e of
      1062 -> "dupkey error occured while inserting/updating"
      1452 -> "foreign key constraints failed inserting/updating"
  , "in function " ++ errFunction e
  , "detailed: " ++ errMessage e
  , "with arg: " ++ show q
  ]



----------------------------------------------------------------------
-- insert che_Professor



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
    return . mapMaybe (fmap conv . nullize) $ names
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
  if isFieldCompatible str then return $ show [PR str]
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
  dbknows <- checkKeyInDB conn (Only name)
  if dbknows then return False else do
    rfname <- refineTmpProf name
    exInsertTmpProf conn (name,rfname)
  where checkKeyInDB conn val = do
          [Only num] <- query conn selq val
          return $ num /= (0 :: Int)
          where selq = "\
            \ SELECT COUNT(name) \
            \ FROM che_professor \
            \ WHERE name = ? \
            \ ;"


-- fills up cache table name conversion (che_professor)
insertTmpProfs :: Connection -> [[String]] -> IO Int
insertTmpProfs conn tbl = countTrue <$>
  mapM (insertTmpProf conn) (transpose (drop 2 tbl) !! 5)



----------------------------------------------------------------------
-- insert Professor


-- insert Teach data to db according to it's position (prof/ta ..)
exInsertTeach :: Connection -> Teach -> IO Bool
exInsertTeach conn teach = case teach of
  (PR s) -> insertNameIfNone "professor" s
  (TA s) -> insertNameIfNone "ta" s
  where
    insertNameIfNone tbl str =
      let insq = mconcat ["INSERT INTO ", tbl, "(name) VALUES (?)"]
          p = Only str
       in exInsertHandler (\q -> execute conn insq q >> return True) p


-- insert names of professors (and TAs!) to db,
mvTmpToProfs :: Connection -> IO Int
mvTmpToProfs conn = do
  -- FIXME should i handle this?
  snamess <- query_ conn selq
  let names = concatMap (read.fromOnly) snamess :: [Teach]
  countTrue <$> mapM (exInsertTeach conn) names
  where selq = "SELECT names FROM che_professor"



----------------------------------------------------------------------
-- insert Course



-- get the sect number from the title field
getCredit :: String -> IO (Maybe Int)
getCredit "" = return Nothing
getCredit str = case parse_credit str of
  Right (_,_,x) -> return (Just x)
  Left _ -> do
    putStrLn "could not parse credit from.."
    putStrLn $ "\"" ++ str ++ "\""
    putStrLn "please enter manually"
    getNumberM "cre: "


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


-- insert single course, with given (unchecked) tuple data of strings
insertCourse :: Connection
             -> (String,String,String,String)
             -> IO Bool
-- school (sch) value is aready in crs, we don't need it
-- also, we don't utilize classify (cls) yet
insertCourse conn (crs,_,tlt',cre') =
  let (tlt,tkr) = refineTlts tlt'
  in do
    cre <- getCredit cre'
    let args = (crs,tlt,tkr,cre)
    exInsertHandler (\q -> execute conn insq q >> return True) args
  where
    insq = "\
      \ INSERT INTO \
      \   course (crs_id, title, title_kr, credit) \
      \ VALUES (?,?,?,?) \
      \ ;"


-- does not depend on che_professor
-- returns the number of courses inserted
insertCourses :: Connection -> [[String]] -> IO Int
insertCourses conn tbl =
  let contbl = drop 2 tbl
      nubtbl = map head $ groupBy ((==) `on` (!!2)) contbl
      -- (crs_id, classify, title', credit')
      tups = map (\s -> (s!!2,s!!1,s!!3,s!!4)) nubtbl
   in countTrue <$> mapM (insertCourse conn) tups



----------------------------------------------------------------------
-- insert Requir


getRequir :: String -> IO [String]
getRequir "" = return []
getRequir str = case parse_requir str of
  Right ls -> return ls
  Left _ -> do
    putStrLn "could not parse requir list from.."
    putStrLn $ "\"" ++ str ++ "\""
    putStrLn "please enter manually"
    promptLinesOf check_crsid
  where check_crsid = right . parse_crsid
        right (Right _) = True
        right _ = False


insertRequir :: Connection -> (String,String)-> IO Int
insertRequir conn (crsid,req')= do
  reqs <- getRequir req'
  let go = exInsertHandler (\q -> execute conn updq q >> return True)
  countTrue <$> mapM (\r -> go (r,crsid)) reqs
  where updq = "\
    \ INSERT INTO relation_requir \
    \ VALUES (?,?) \
    \ ;"


insertRequirs :: Connection -> [[String]] -> IO Int
insertRequirs conn tbl =
  let contbl = drop 2 tbl
      nubtbl = map head $ groupBy ((==) `on` (!!2)) contbl
      -- (crs_id, requir')
      reqs = map (\s -> (s!!2,s!!9)) nubtbl
   in sum <$> mapM (insertRequir conn) reqs



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


getEnrol :: String -> IO (Maybe Int)
getEnrol "" = return Nothing
getEnrol str = case reads str :: [(Int,String)] of
  [(a,'명':_)] -> return $ Just a
  _ -> do
    putStrLn "could not parse enrol number from.."
    putStrLn $ "\"" ++ str ++ "\""
    putStrLn "please enter manually"
    getNumberM "enrol: "


getCacheProf :: Connection -> String -> IO [Teach]
getCacheProf conn str = do
  sel <- query conn selq (Only str)
  case sel of
    Only x : _ -> return.read $ x
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
      -- shouldn't we show the name title or something?
      putStrLn "could not get pr/ta properly from cache.."
      putStrLn $ "\"" ++ str ++ "\""
      putStrLn "please enter manually"
      getManualTeachPair


insertSect :: Connection -> Int -> (String,String,String,String,Int)
             -> IO Bool
-- we can check secn (int) with sectno (string) but we don't..
insertSect conn secn (_,crsid,prof',enrol',sme) = do
  enrol <- getEnrol enrol'
  (prof,ta) <- getProfFromTmp conn prof'
  let args = (secn,crsid,prof,ta,enrol,sme)
  exInsertHandler (\q -> execute conn insq q >> return True) args
  where insq = "\
    \ INSERT INTO \
    \   section (sect_no, crsid, prof, ta, enroll_size,semester) \
    \ VALUES (?,?,?,?,?,?); \
    \ "


insertSectGrp :: Connection -> [(String,String,String,String,Int)] -> IO Int
insertSectGrp conn grps = countTrue <$>
  zipWithM (insertSect conn) [1..] grps


insertSects :: Connection -> [[String]] -> IO Int
insertSects conn tbl =
  -- (sect#, crsid, prof(ta), enroll)
  let contbl = drop 2 tbl
      tups = map (\s -> (s!!7,s!!2,s!!5,s!!8)) contbl
      gtups = groupBy ((==) `on` snd4) tups
  in do
    sme <- getNumber "semester code: "
    sum <$> mapM (insertSectGrp conn . map (apn4 sme)) gtups
  where snd4 (_,b,_,_) = b
        apn4 e (a,b,c,d) = (a,b,c,d,e)



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
getSectno :: String -> IO Int
getSectno str =
  case dropWhile (/='(') str of
    [] -> return 1
    '(':xs -> case reads xs of
      [(a,'반':_)] -> return a
      _ -> do
        putStrLn "could not parse sect number from.."
        putStrLn $ "\"" ++ str ++ "\""
        putStrLn "please enter manually"
        getNumber "cre: "


-- merely excutes the query to db
-- (room, crsid, sectno)
-- always success (since it's update
exInsertRoom :: Connection -> (String,String,Int) -> IO Bool
exInsertRoom conn arg =
  -- FIXME not doing any exception handling
  -- since this is not a 'insertion', we can't use InsertHandler
  -- nothing happens if (crsid,sectno) not found in db
  exInsertHandler (\q -> execute conn updq q >> return True) arg
  where updq = "\
    \ UPDATE section \
    \ SET room = ? \
    \ WHERE crsid = ? AND sect_no = ? \
    \;"


-- update table 'section' with room the infomation
insertRooms :: Connection -> Pcells -> IO Int
insertRooms conn pcells =
  let vcells = filterNoRoom . concat . concat $ pcells
   in countTrue <$> mapM (g >=> exInsertRoom conn) vcells
  where filterNoRoom = filter f
        f (_,_,_,d) = not $ null d
        g (a,b,c,d) = do
          sectno <- getSectno c
          return (d,a,sectno)



----------------------------------------------------------------------
-- insert TimeTable



exInsertTmt :: Connection -> (String,Int,String, Int) -> IO Bool
exInsertTmt conn arg =
  exInsertHandler (\q -> execute conn insq q >> return True) arg
  where insq = "\
    \ INSERT INTO \
    \   class (crsid,sectno,day,period) \
    \ VALUES (?,?,?,?) \
    \;"


insertVcells :: Connection -> Int -> Int -> Vcells -> IO Int
insertVcells conn np nd vcells =
  let days = ["MON", "TUE", "WED", "THR", "FRI", "SAT", "SUN"]
      (d,p) = (days !! (nd-1), np)
  in countTrue <$> mapM (insvc p d >=> exInsertTmt conn) vcells
  where insvc p d (a,_,c,_) = do
          sectno <- getSectno c
          return (a,sectno,d,p)


insertTmts :: Connection -> Pcells -> IO Int
insertTmts conn pcells =
  sum . concat <$> zipWithM insp [1..] (filterEmpty pcells)
  where insp p = zipWithM (insertVcells conn p) [1..]
