
module Types (

  {- NOTION of constructor hiding
   - there are some exporting their constructors
   - because they can verify their validity
   - with its own constructors only
   - others are not e.g. Crsid (GS,999999) is not valid but possible
   - and does not export there constructors
   -}

  -- basic types (fields)
    Season(..)

  , WeekDay(..)
  ,   weekdayTstring
  ,   stringTweekday

  , Semester()
  ,   semesterTint
  ,   intTsemester

  , School(..)
  ,   schoolTstring
  ,   stringTschool

  , Crsid()
  ,   crsidTstring
  ,   stringTcrsid

  , Lectime()
  , LecTime()

  , RoomId() -- for future safety

  -- db table types
  , Prof()
  , TeachAssi()
  , Course()
  , Section()

  , RefCrs()
  , RefSect()

  ) where

import Types.Internal
