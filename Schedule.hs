{-# LANGUAGE OverloadedStrings #-}

import Data.Dates
import Data.Aeson
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Function

-- NOTICE every data type is deriving some typeclasses for debuging convenience, they the features of those (like show function for Show typeclass) should not be utilized

-- for natural seasons
data Season = Spring | Summer | Fall | Winter deriving Show

-- Semester {year seaon}
data Semester = Semester Int Season deriving Show

-- FIXME this format ignores prefix (GS in GS2254)
type Crsid = Int

-- TODO yet to add more classifications of courses
data Classify = PPE | Nill deriving Show

-- Lectime {Monday 3} : third lecture time on Monday
data Lectime = Lectime WeekDay Int deriving Show

-- RoomId = "A331" | "EECS-A 210"
-- FIXME got better idea for roomid format?
type RoomId = T.Text

data Prof =
  Prof { prof_name   :: T.Text
       , prof_roomid :: RoomId
       , prof_full   :: Bool -- whether full or part time
       } deriving Show

data School = EECS deriving Show

data Course =
  Course { crs_id   :: Crsid
         , school   :: School
         , clsf     :: Classify
         , title    :: T.Text
         , title_kr :: T.Text
      -- , hrs      :: Int
      -- , e        :: Int
         , credit   :: Int
         , requir   :: Crsid
         , semester :: Semester
         } deriving Show

data Section = 
  Section { crsid   :: Crsid
          , sectno  :: Int
          , prof    :: Prof
          , lectime :: Set Lectime
          , roomid  :: RoomId
          , enroll_size :: Int
          } deriving Show

-- Treating its primary keys (course id, section #) only
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
    object [ "crsid" .= crsid rs, "sectno" .= sectno rs ]
