{-# LANGUAGE OverloadedStrings #-}

module Uploader
  ( insertProfs
  , insertCourses
  , insertSect
  , insertRooms
  , insertTmts
  )
  where

import Data.List (transpose, groupBy)
import Data.Char
import Data.Function (on)
import Data.Bool (bool)
import Data.Maybe (listToMaybe)
import Control.Monad.Trans.Except
import Control.Monad (when,guard)
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryParams (QueryParams)

import Parser (parse_requir)


----------------------------------------------------------------------
-- insert Professor



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


-- insert name to db if there's no pre-inserted
-- prevent sql-error exceptions
-- FIXME this might be not appropriate if the name field is
-- not primary key anymore
executeIfNone :: (QueryParams q1, QueryParams q2)
             => Connection -> Query -> Query -> q1
             -> Query -> q2 -> IO Bool
executeIfNone conn tbl key val exq p = do
  dbknows <- checkKeyInDB conn tbl key val
  when (not dbknows) $
    execute conn exq p >> return ()
  return (not dbknows)


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
insertTmpProf :: Connection -> String -> IO ()
insertTmpProf conn name = do
  dbknows <- (checkKeyInDB conn "tmp_professor" "name" (Only name))
  when (not dbknows) $ do rfname <- refineTmpProf name
                          exInsertTmpProf conn (name,rfname)


-- insert Teach data to db according to it's position (prof/ta ..)
exInsertTeach :: Connection -> Teach -> IO ()
exInsertTeach conn teach = (case teach of
  (PR s) -> insertNameIfNone conn "professor" s
  (TA s) -> insertNameIfNone conn "ta" s
  ) >> return ()
  where
    insertNameIfNone conn tbl str =
      let exq = mconcat ["INSERT INTO ", tbl, "(name) VALUES (?)"]
          nameParam = (Only str)
       in executeIfNone conn tbl "name" nameParam exq nameParam



-- assume tmp_proff exist, move the data from - to prof, ta db
mvTmpToProfs :: Connection -> IO Int
mvTmpToProfs conn = do
  snamess <- query_ conn selq
  let names = concat (map (read.fromOnly) snamess :: [[Teach]])
   in mapM_ (exInsertTeach conn) names >> return (length names)
  where selq = "SELECT names FROM tmp_professor"


-- top level of inserting to table 'professor'
-- insert names of professors (and TAs!) to db,
-- also leaves name conversion cache table (tmp_professor)
-- for use of another (course, section ..) parsing
-- FIXME supossed to return the number of names inserted..
-- but it doesn't match
insertProfs :: Connection -> [[String]] -> IO Int
insertProfs conn tbl = do
  execute_ conn createq
  mapM (insertTmpProf conn) $ transpose (drop 2 tbl) !! 5
  mvTmpToProfs conn
  where createq = "\
    \ CREATE TEMPORARY TABLE tmp_professor ( \
    \ name NVARCHAR(50) NOT NULL PRIMARY KEY, \
    \ names NVARCHAR(200) NOT NULL \
    \ );"



----------------------------------------------------------------------
-- insert Course



-- span, omits the separator
omitSpan :: (a -> Bool) -> [a] -> ([a],[a])
omitSpan f ls = case span f ls of (as,c:bs) -> (as,bs)
                                  x         -> x


boolMaybe :: (a -> Bool) -> a -> Maybe a
boolMaybe f a = guard (f a) >> return a


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd f = reverse . takeWhile f . reverse


-- gets the strings to insert db from parsed one
refineTlts :: String -> (String,String)
refineTlts tlt' = let (tkr,tlt) = omitSpan (/='\n') tlt'
                   in (strip . map sub $ tlt, strip tkr)
  where
    lstrip = dropWhile isSpace
    strip = lstrip . reverse . lstrip . reverse
    sub c | isAscii c = c
              | otherwise = case c of
                  'Ⅱ' -> '2'
                  'Ⅰ' -> '1'
                  _   -> error $ '"' : (show c) ++ "\""


-- merely excutes the insert query to db
exInsertCourse :: Connection
               -> (String,String,String,Int
                  ,Int,Maybe String,Maybe String,Maybe String)
               -> IO Bool
exInsertCourse conn args@(ii,_,_,_,_,_,_,_) =
  executeIfNone conn "course" "crs_id" (Only ii) insq args
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
      cre = read $ takeWhileEnd (/=':') cre' :: Int
      -- FIXME this should be able to set manually
      sme = 216
      -- ignore parsing error
      reqs = case parse_requir req' of Right ls -> ls ++ ["","",""]
      rqs = map (boolMaybe $ not.null) reqs
      rq1 = rqs !! 0
      rq2 = rqs !! 1
      rq3 = rqs !! 2
   in exInsertCourse conn (crs,tlt,tkr,cre,sme,rq1,rq2,rq3)


-- top level of inserting to table 'course'
-- does not depend on tmp_professor
-- returns the number of courses inserted
insertCourses :: Connection -> [[String]] -> IO Int
insertCourses conn tbl =
  -- (crs_id, classify, title', credit', requir')
  let contbl = drop 2 tbl
      nubtbl = map head $ groupBy ((==) `on` (!!3)) contbl
      tups = map (\s -> (s!!2,s!!1,s!!3,s!!4,s!!9)) nubtbl
      num = length tups
   in fmap (sum . map (bool 0 1)) $ mapM (insertCourse conn) tups



----------------------------------------------------------------------
-- insert Section


type Good = ExceptT String IO ()

infix 0 <~~

--(<~~) :: Monad m => m Bool -> String -> Good
(<~~) :: IO Bool -> String -> Good
mb <~~ str = ExceptT $ do
  b <- mb
  return $ if b then Right () else Left str

nope :: Monad m => m Bool -> m Bool
nope = fmap not

isGood :: Either String () -> Bool
isGood (Right ()) = True
isGood (Left _) = False

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

getProfFromTmp :: Connection -> String -> IO (Maybe String, Maybe String)
getProfFromTmp conn str = do
  -- unique tmp_prof existence vertified from insertSect
  -- FIXME isn't this inefficient?
  [Only names] <- query conn selq (Only str)
  ts <- return.read $ names :: IO [Teach]
  case ts of []             -> return (Nothing, Nothing)
             [PR pr]        -> return (Just pr, Nothing)
             [TA _]         -> error "only ta??"
             [PR pr, TA ta] -> return (Just pr, Just ta)
             [TA ta, PR pr] -> return (Just pr, Just ta)
             _ -> promptTeachers ts

  where
    selq = "SELECT names FROM tmp_professor WHERE name = ?"
    promptTeachers ts = do
      putStrLn $ "prompt for \"" ++ str ++ "\""
      putStr "pr: "
      pr <- getLine
      putStr "ta: "
      ta <- getLine
      return (boolMaybe (not.null) pr, boolMaybe (not.null) ta)


exInsertSect :: Connection -> Int -> (String,String,String,String) -> IO ()
-- we can check secn (int) with sectno (string) but we don't..
exInsertSect conn secn (_,crsid,prof',enrol') =
  let enrol = fmap fst . listToMaybe $ (reads enrol' :: [(Int,String)])
  in do
    (prof,ta) <- getProfFromTmp conn prof'
    execute conn insq (secn,crsid,prof,ta,enrol)
    return ()
  where insq = "\
    \ INSERT INTO \
    \   section (sect_no, crsid, prof, ta, enroll_size) \
    \ VALUES (?,?,?,?,?); \
    \ "


insertSectGrp :: Connection -> [(String,String,String,String)] -> IO ()
insertSectGrp conn grps@((_,crsid,prof,_):_) =
  let isdup = checkKeyInDB conn "section" "crsid" $ Only crsid
      iscrs = checkKeyInDB conn "course" "crs_id" $ Only crsid
      isprf = checkKeyInDB conn "tmp_professor" "name" $ Only prof
      scrsid = '(' : show crsid ++ ")"
      sprof = '(' : show prof ++ ")"
      ready = do
        nope isdup <~~ "there's section of same crsid " ++ scrsid
        iscrs      <~~ "there's no such crsid " ++ scrsid
        isprf      <~~ "there's no such prof " ++ sprof
      ex = mapM_ (uncurry $ exInsertSect conn) $ zip [1..] grps
   in do r <- runExceptT ready
         when (isGood r) ex


--FIXME
--insertSect :: Connection -> [[String]] -> IO Int
insertSect :: Connection -> [[String]] -> IO ()
insertSect conn tbl =
  -- (sect#, crsid, prof(ta), enroll)
  let contbl = drop 2 tbl
      tups = map (\s -> (s!!7,s!!2,s!!5,s!!8)) contbl
      gtups = groupBy ((==) `on` snd4) tups
   in mapM_ (insertSectGrp conn) gtups
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


-- get the sect number from the name field of vcell
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
