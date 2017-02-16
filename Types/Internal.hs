{-# LANGUAGE OverloadedStrings #-}

module Types.Internal where

import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function
import Control.Monad


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


-- Semester {year, seaon}
data Semester = Semester Int Season
  deriving (Eq, Show, Read, Ord)


-- School department
-- TODO this can vary on schools
--  type School = Char[2] ?
data School = GS | PS | CH | BS | EC | MC | MA | EV
  deriving (Eq, Show, Read, Ord, Enum, Bounded)


-- required by tojson instance of crsid
instance ToJSON School where
  toJSON = toJSON . show


-- required by tojson instance of crsid
instance FromJSON School where
  parseJSON (String s) = case s of
    "GS" -> return GS
    "PS" -> return PS
    "CH" -> return CH
    "BS" -> return BS
    "EC" -> return EC
    "MC" -> return MC
    "MA" -> return MA
    "EV" -> return EV
    _    -> mzero


-- course id (e.g. GS1101)
-- TODO what if Int < 1000 ?
--  GS011 -> GS0011 ?
type Crsid = (School,Int)


-- Lectime {Monday 3} : third lecture time on Monday
-- almost every class starts at regular time, so those
-- classes having non-regular length of lecture time
-- can be represented two or more lectimes
data Lectime = Lectime WeekDay Int
  deriving (Eq, Show, Read, Ord)


-- room name (e.g. "대학A 105")
type RoomId = Text


-- professor data
data Prof = Prof
  { prof_name   :: Text
  , prof_office :: RoomId
  , prof_full   :: Bool -- whether full or part time
  , prof_email  :: Text
  } deriving Show


-- ta data TeachAssi {name}
data TeachAssi = TeachAssi Text
  deriving Show


-- course data
data Course = Course
  { crs_id   :: Crsid
  , title    :: Text
  , title_kr :: Text
  , credit   :: Int
  , requir   :: Set Crsid
  } deriving Show


-- section data
data Section = Section
  { crsid   :: Crsid
  , sectno  :: Int
  , prof    :: Prof
  , ta      :: TeachAssi
  , lectime :: [Lectime]
  , roomid  :: RoomId
  , enroll_size :: Int
  , semester :: Semester
  } deriving Show


-- wrap Course to treate it by (only) it's primary key (crsid)
newtype RefCrs = RefCrs Course

instance Eq RefCrs where
  (RefCrs a) == (RefCrs b) =
    ((==) `on` crs_id) a b

instance Ord RefCrs where
  (RefCrs a) <= (RefCrs b) =
    ((<=) `on` crs_id) a b

instance Show RefCrs where
  show (RefCrs a) = "{crs = " ++ show (crs_id a) ++ "}"

instance ToJSON RefCrs where
  toJSON (RefCrs rc) = toJSON (crs_id rc)


-- wrap Section to treate it by (only) it's primary key (crsid, sectno)
newtype RefSect = RefSect Section

instance Eq RefSect where
  (RefSect a) == (RefSect b) =
    ((==) `on` crsid) a b ||
    ((==) `on` sectno) a b

instance Ord RefSect where
  (RefSect a) <= (RefSect b)
    | ((>) `on` crsid) a b = False
    | ((<) `on` crsid) a b = True
    | otherwise = ((<=) `on` sectno) a b

instance Show RefSect where
  show (RefSect a) = "{crs = " ++ show (crsid a)
    ++ ", sct = " ++ show (sectno a) ++ "}"

instance ToJSON RefSect where
  toJSON (RefSect rs) =
    object [ "crsid" .= crsid rs
           , "sectno" .= sectno rs ]
