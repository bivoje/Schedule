{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

--import Data.Function
--import Data.List (splitAt)
--import Data.Char (isNumber)
--import Data.Maybe (fromMaybe)
--import Control.Monad
--import Control.Exception (assert,throw)

import Data.String

{-
 - WARNING!!
 - most of the types in this module derive Show/Read instance.
 - But it's only for debugging purpose and should not
 - be utilized in the actual code
-}


{- represents most of the string types (String, Text, ByteString, etc)
 - IsString for string literal
 - Eq for pattern matching
 - Monoid for appending
 - probably Text will be used for the most of the time than other
 - to be efficient (string) and encoding-safe (bytestring)
 -}
class (IsString s, Eq s, Monoid s) => IString s

instance IString String where

--instance IString Text where

--instance IString LT.Text where

--instance IString ByteString where


-- (.) for binary function
wrap :: (c -> d) -> (a -> b -> c) -> a -> b -> d
wrap f g a b= f (g a b)

-- it should have imported with Data.Monoid
-- but for some reason, it haven't
infixr 6 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend


------------------------------------------------------------------
-- Primitive types

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
weekdayTstr :: IString s => WeekDay -> s
weekdayTstr s = case s of {
  Monday    -> "MON"; Tuesday   -> "TUE"; Wednesday -> "WED";
  Thursday  -> "THU"; Friday    -> "FRI"; Saturday  -> "SAT";
  Sunday    -> "SUN";
}

-- parses the weekday data from db
strTweekday :: IString s => s -> Maybe WeekDay
strTweekday s = case s of {
  "MON" -> Just Monday   ; "TUE" -> Just Tuesday  ; "WED" -> Just Wednesday;
  "THU" -> Just Thursday ; "FRI" -> Just Friday   ; "SAT" -> Just Saturday ;
  "SUN" -> Just Sunday   ; _ -> Nothing
}

-- winter semester of 2018 -> 184
newtype Semester = Semester Int
  deriving (Eq, Show, Read)

-- School department
--  type School = Char[2] ?
newtype Major = Major String
  deriving (Eq, Show, Read, Ord)

-- course id (e.g. GS1101)
data Crsid = Crsid { getMajor :: Major, getCode :: Int }
  deriving (Eq, Show, Read, Ord)

-- section id (e.g. GS1101-1)
-- constructor is hidden to outside
data Sectid = Sectid { crsid :: Crsid, sectno :: Int }
  deriving (Eq, Show, Read, Ord)

-- Lectime {Monday 3} : third lecture time on Monday
-- almost every class starts at regular time, so those
-- classes having non-regular length of lecture time
-- can be represented two or more lectimes
data Lectime = Lectime WeekDay Int
  deriving (Eq, Show, Read, Ord)

-- Lectime wraper that ordered by time period first
-- Lectime Trans :> LectimeT :> LecTime
--newtype LecTime = LecTime Lectime
--  deriving (Eq, Show, Read)
--
--instance Ord LecTime where
--  (LecTime (Lectime w1 n1)) <= (LecTime (Lectime w2 n2)) =
--    if n1 == n2 then w1 <= w2 else n1 < n2

-- room name (e.g. "대학A 105")
type RoomId = String

-- professor data
data Prof = Prof
  { prof_name   :: String
  , prof_office :: RoomId
  , prof_full   :: Bool -- whether full or part time
  , prof_email  :: String
  } deriving (Show, Read)

data TeachAssi = TeachAssi String
  deriving (Show, Read)

data Course = Course
  { crs_id   :: Crsid
  , title    :: String
  , title_kr :: String
  , credit   :: Int
  } deriving (Show, Read)

data Section = Section
  { sect_id :: Sectid
  , prof    :: Prof
  , ta      :: TeachAssi
  , lectime :: [Lectime]
  , roomid  :: RoomId
  , enroll_size :: Int
  , semester :: Semester
  } deriving (Show, Read)
