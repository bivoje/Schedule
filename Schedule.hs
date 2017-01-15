import Data.Dates

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
data Lectime = Lectiom WeekDay Int deriving Show

-- RoomId = "A331" | "EECS-A 210"
-- FIXME got better idea for roomid format?
type RoomId = String

data Prof = Prof String String deriving Show

data Course =
  Course { id       :: Crsid
         , clsf     :: Classify
         , title    :: String
         , title_kr :: String
         , credit   :: Int
         , requir   :: Crsid
         , semester :: Semester
         } deriving Show

data Section = 
  Section { crsid   :: Crsid
          , sectno  :: Int
          , prof    :: Prof
          , lectime :: Lectime
          , room    :: RoomId
          } deriving Show
