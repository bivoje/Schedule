{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Internal where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Function
import Data.List (splitAt)
import Data.Char (isNumber)
import Control.Monad
import Control.Exception (assert)

import Data.String

{-
 - WARNING!!
 - most of the types in this module derive Show/Read instance.
 - But it's only for debugging purpose and should not
 - be utilized in the actual code
-}


-- represents most of the string types (String, Text, ByteString, etc)
-- IsString for string literal
-- Eq for pattern matching
-- Monoid for appending
-- probably Text will be used for the most of the time than other
-- to be efficient (string) and encoding-safe (bytestring)
class (IsString s, Eq s, Monoid s) => IString s

instance IString String where

instance IString Text where

instance IString ByteString where


-- (.) for binary function
wrap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
wrap f g a b= f (g a b)

-- it should have imported with Data.Monoid
-- but somehow it haven't
infixr 6 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend


-- 4 seasons(terms) of semester
-- TODO what about 3 terms?
--  omit Winter or something?
{- consider this table
                                    3 학기제 (3 Terms)
  가을 학기 (Autumn Term)         봄 학기 (Spring Term)           여름 학기 (Summer Term)
  9월 초순-12월 초순              1월 초순-3월 하순               4월 초순-6월 하순
  Half Term (중간에 일주일 방학)  Half Term (중간에 일주일 방학)  Half Term (중간에 일주일 방학)
  from http://www.peopleloving.co.kr/university/0101.html
-}
data Season
  = Spring
  | Summer
  | Autumn
  | Winter
  deriving (Eq, Show, Read, Ord, Enum, Bounded)


-- same as in Data.Dates
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- convert to string as saved in db
weekdayTstring :: IString s => WeekDay -> s
weekdayTstring s = case s of {
  Monday    -> "MON"; Tuesday   -> "TUE"; Wednesday -> "WED";
  Thursday  -> "THU"; Friday    -> "FRI"; Saturday  -> "SAT";
  Sunday    -> "SUN";
}

-- parses the weekday data from db
stringTweekday :: IString s => s -> Maybe WeekDay
stringTweekday s = case s of {
  "MON" -> Just Monday   ; "TUE" -> Just Tuesday  ; "WED" -> Just Wednesday;
  "THU" -> Just Thursday ; "FRI" -> Just Friday   ; "SAT" -> Just Saturday ;
  "SUN" -> Just Sunday   ; _ -> Nothing
}


-- Semester {year, seaon}
data Semester = Semester Int Season
  deriving (Eq, Show, Read, Ord)

-- convert to string as saved in db
-- e.g. Semester 17 Winter -> 417
-- semester assumed to have valid values
semesterTint :: Semester -> Int
semesterTint (Semester n s) = assert (0 <= n && n < 100) $
  ((fromEnum s + 1) * 100) + n

-- parses the semester data from db
-- e.g. 417 -> Semester 17 Winter
intTsemester :: Int -> Maybe Semester
intTsemester i
  | i < 100 = Nothing
  | i `div` 100 > (fromEnum (maxBound :: Season) +2) = Nothing
  | otherwise = Just . Semester (mod i 100) . toEnum $ div i 100 - 1


-- School department
-- TODO this can vary on schools
--  type School = Char[2] ?
data School = GS | PS | CH | BS | EC | MC | MA | EV
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- convert to string as saved in db
schoolTstring :: IString s => School -> s
schoolTstring s = case s of {
   GS -> "GS"; PS -> "PS"; CH -> "CH"; BS -> "BS";
   EC -> "EC"; MC -> "MC"; MA -> "MA"; EV -> "EV";
}

-- parses the school data from db
stringTschool :: IString s => s -> Maybe School
stringTschool s = case s of {
  "GS" -> Just GS; "PS" -> Just PS; "CH" -> Just CH; "BS" -> Just BS;
  "EC" -> Just EC; "MC" -> Just MC; "MA" -> Just MA; "EV" -> Just EV;
  _    -> Nothing;
}

-- required by tojson instance of crsid
instance ToJSON School where
  toJSON = toJSON . (schoolTstring :: School -> Text)

-- required by tojson instance of crsid
instance FromJSON School where
  parseJSON (String s) = maybe mzero return . stringTschool $ s
  parseJSON _ = fail "expected School code"


-- course id (e.g. GS1101)
-- constructor is hidden to outside
newtype Crsid = Crsid (School,Int)
  deriving (Eq, Show, Read, Ord)

-- convert to string as saved in db
-- e.g. (GS,101) -> "GS0101"
-- crsid assumed to have valid values
crsidTstring :: IString s => Crsid -> s
crsidTstring (Crsid (sc,n)) = assert (0 <= n && n < 10000) $
  schoolTstring sc <> fromString (swrap n)
  where swrap = reverse . take 4 . reverse . (zeros ++) . show
        zeros = replicate 4 '0'

-- parses the school data from db
-- e.g. (GS,1101) -> "GS1101"
stringTcrsid :: String -> Maybe Crsid
stringTcrsid s =
  let (sc,n) = splitAt 2 s
  in Crsid `wrap` (,) <$> stringTschool sc
                      <*> f n
  where -- efficiently (lazily?) check if the length is 4
    f s@[_,_,_,_] | all isNumber s = Just (read s)
    f s = Nothing

-- required by tojson instance of refcrs
instance ToJSON Crsid where
  toJSON (Crsid c) = toJSON c

-- required by tojson instance of refsect
instance FromJSON Crsid where
  parseJSON x = Crsid <$> parseJSON x


-- Lectime {Monday 3} : third lecture time on Monday
-- almost every class starts at regular time, so those
-- classes having non-regular length of lecture time
-- can be represented two or more lectimes
data Lectime = Lectime WeekDay Int
  deriving (Eq, Show, Read, Ord)

-- Lectime wraper that ordered by time period first
-- Lectime Trans :> LectimeT :> LecTime
newtype LecTime = LecTime Lectime
  deriving (Eq, Show, Read)

instance Ord LecTime where
  (LecTime (Lectime w1 n1)) <= (LecTime (Lectime w2 n2)) =
    if n1 == n2 then w1 <= w2 else n1 < n2

type LectimeSet = Set Lectime
type LecTimeSet = Set LecTime

-- room name (e.g. "대학A 105")
type RoomId = Text


-- professor data
data Prof = Prof
  { prof_name   :: Text
  , prof_office :: RoomId
  , prof_full   :: Bool -- whether full or part time
  , prof_email  :: Text
  } deriving (Show, Read)


-- ta data TeachAssi {name}
data TeachAssi = TeachAssi Text
  deriving (Show, Read)


-- course data
data Course = Course
  { crs_id   :: Crsid
  , title    :: Text
  , title_kr :: Text
  , credit   :: Int
  , requir   :: Set Crsid
  } deriving (Show, Read)


-- section data
data Section = Section
  { crsid   :: Crsid
  , sectno  :: Int
  , prof    :: Prof
  , ta      :: TeachAssi
  , lectime :: LectimeSet
  , roomid  :: RoomId
  , enroll_size :: Int
  , semester :: Semester
  } deriving (Show, Read)


-- wrap Course to treat it by (only) it's primary key (crsid)
newtype RefCrs = RefCrs Course
  deriving (Show, Read)

instance Eq RefCrs where
  (RefCrs a) == (RefCrs b) =
    ((==) `on` crs_id) a b

instance Ord RefCrs where
  (RefCrs a) <= (RefCrs b) =
    ((<=) `on` crs_id) a b

instance ToJSON RefCrs where
  toJSON (RefCrs rc) = toJSON (crs_id rc)


-- wrap Section to treat it by (only) it's primary key (crsid, sectno)
newtype RefSect = RefSect Section
  deriving (Show, Read)

instance Eq RefSect where
  (RefSect a) == (RefSect b) =
    ((==) `on` crsid) a b ||
    ((==) `on` sectno) a b

instance Ord RefSect where
  (RefSect a) <= (RefSect b)
    | ((>) `on` crsid) a b = False
    | ((<) `on` crsid) a b = True
    | otherwise = ((<=) `on` sectno) a b

instance ToJSON RefSect where
  toJSON (RefSect rs) =
    object [ "crsid" .= crsid rs
           , "sectno" .= sectno rs ]
