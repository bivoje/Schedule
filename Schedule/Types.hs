
module Schedule.Types (

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
  ,   weekdayTstr
  ,   strTweekday

  , Semester()
  ,   semesterTint
  ,   intTsemester

  , School(..)
  ,   schoolTstr
  ,   strTschool

  , Crsid()
  ,   school
  ,   code
  ,   crsidTstr
  ,   strTcrsid
  , CrsidSet

  , Sectid()
  ,   crsid
  ,   sectno
  ,   sectidTstr
  ,   strTsectid

  , Lectime()
  , LecTime()

  , RoomId() -- for future safety

  -- db table types
  , Prof()
  ,   prof_name
  ,   prof_office
  ,   prof_full
  ,   prof_email

  , TeachAssi()

  , Course()
  ,   crs_id
  ,   title
  ,   title_kr
  ,   credit

  , Section()
  ,   sect_id
  ,   prof
  ,   ta
  ,   lectime
  ,   roomid
  ,   enroll_size
  ,   semester

  ) where

import Schedule.Types.Internal
