{-# LANGUAGE OverloadedStrings #-}

module Schedule.DBServer (
    getConnection
  , getLectime
  , getRequir
  , getProf
  , getCrs
  , getSect
  ) where

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Set as S (fromList)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.Aeson
import Database.MySQL.Simple
import System.IO
import Control.Applicative ((<|>))
import Control.Exception (bracket_)

import Schedule.Types.Internal
{- DBServer module imports Types.Internal directly
 - and provides getter-functions of some composite types.
 - Using those functions would guarantee the validity of the data.
 -
 - Paradigm in types of all the getters is primitive.
 - They take and return minimal information for the given task.
 - for example getRequir has type of 'Crsid -> IO CrsidSet'
 - not 'Crsid -> IO [Course]' nor 'Course -> IO [Course]'.
 -}


instance FromJSON ConnectInfo where
  parseJSON (Object v) =
    let fields = ["host", "port", "user", "pswd", "dtbs", "path"]
        seq = [ rec1, rec2, rec3, rec4, rec5, rec6 ]
        f = not . flip elem fields
        valid = H.null $ H.filterWithKey (f `wrap` const) v
    in if not valid then fail "invaild field for connectInfor"
       else foldl (\c r -> (c >>= r) <|> c) (return defaultConnectInfo) seq
    where
      rec1 c = (\x -> c { connectHost     = x }) <$> v .: "host"
      rec2 c = (\x -> c { connectPort     = x }) <$> v .: "port"
      rec3 c = (\x -> c { connectUser     = x }) <$> v .: "user"
      rec4 c = (\x -> c { connectPassword = x }) <$> v .: "pswd"
      rec5 c = (\x -> c { connectDatabase = x }) <$> v .: "dtbs"
      rec6 c = (\x -> c { connectPath     = x }) <$> v .: "path"

  parseJSON _ = fail "expected "


getConnection :: IO Connection
getConnection = do
  json <- B.readFile "config.json"
  case eitherDecode json of
    Left str -> error str
    Right c -> connect c


-- gets Lectimes related with given crsid and sect# from the server
-- returns empty set if no lectime found
getLectime :: Connection -> Sectid -> IO LectimeSet
getLectime conn (Sectid c n) = S.fromList . map (uncurry Lectime)
  <$> query conn selq (crsidTstr c :: Text, n)
  where selq = "\
    \ SELECT day, period \
    \ FROM class \
    \ WHERE crsid = ? AND sectno = ? \
    \ ;"


-- returns all the crsid that is in requir-relation with given crs
-- all x where (x ~> crs), c.f. (x, crs)
getRequir :: Connection -> Crsid -> IO CrsidSet
getRequir conn crs =
  S.fromList . map fromOnly <$> query conn selq (Only crs)
  where selq = "\
    \ SELECT requir \
    \ FROM relation_requir \
    \ WHERE target = ? \
    \ ;"


-- gets Prof information for given name from the server
-- returns Nothing if sectiong with sectid not exist in DB
getProf :: Connection -> Text -> IO (Maybe Prof)
getProf conn name = do
  x <- query conn selq (Only name)
  -- there will be one or zero result since crsid is primary
  return $ case map nulting x of
    [] -> Nothing          -- nothing on database
    [(off,ful,eml)] -> Just $ Prof name off ful eml
  where
    nulting (a,b,c) = (nult a, nult' b, nult c)
    nult = fromMaybe ""
    nult' = fromMaybe False
    selq = "\
      \ SELECT office, full, email \
      \ FROM professor \
      \ WHERE name = ? \
      \ ;"


-- gets Course with given crsid from the server
-- returns Nothing if sectiong with sectid not exist in DB
getCrs :: Connection -> Crsid -> IO (Maybe Course)
getCrs conn c = do
  -- there will be one or zero result since crsid is primary
  x <- query conn selq $ Only (crsidTstr c :: Text)
  return $ case x of
    [] -> Nothing
    -- all fields are not null, we don't check nullity
    [(tlt,tlk,cre)] -> Just $ Course {
      crs_id = c, title = tlt, title_kr = tlk, credit = cre
    }
  where
    selq = "\
      \ SELECT title, title_kr, credit \
      \ FROM course \
      \ WHERE crs_id = ? \
      \ ;"


-- gets Section with given sectid from the server
-- returns Nothing if sectiong with sectid not exist in DB
getSect :: Connection -> Sectid -> IO (Maybe Section)
getSect conn s@(Sectid c n) = do
  x <- query conn selq (crsidTstr c :: Text, n)
  -- there will be one or zero result since crsid is primary
  case map nulting x of
    [] -> return Nothing          -- nothing on database
    [(prf,t,rom,sz,sme)] -> do
      ltm <- getLectime conn s
      return . Just $ Section {
        sect_id = s, prof = prf, ta = t, roomid = rom,
        enroll_size = sz, semester = sme, lectime = ltm
      }
  where
    nulting (a,b,c,d,e) = (nult a, nult b, nult c, nult' d, e)
    nult = fromMaybe ""
    nult' = fromMaybe 0
    selq = "\
      \ SELECT prof, ta, room, enroll_size, semester \
      \ FROM section \
      \ WHERE crsid = ? AND sect_no = ? \
      \ ;"
