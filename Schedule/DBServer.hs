{-# LANGUAGE OverloadedStrings #-}

module Schedule.DBServer (
    getConnection
  , getLectime
  , getRequir
  , getProf
  , getRefCrs
  , getRefSect
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Set as S (fromList)
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Lazy as H
import Data.Aeson
import Database.MySQL.Simple
import System.IO
import Control.Applicative ((<|>))
import Control.Exception (bracket_)
import Control.Monad.Trans.Maybe

import Schedule.Types.Internal
{- DBServer module imports Types.Internal directly
 - and provides getter-functions of some composite types.
 - Using those functions would guarantee the validity of the data.
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
-- returns Nothing if server contains invalid value (mostly weekday)
-- this occurs because db (mysql) does not support 'check' clause
getLectime :: Connection -> (Crsid,Int) -> IO (Maybe LectimeSet)
getLectime conn (c,n) =
  fmap S.fromList . mapM (\(w,p) -> flip Lectime p <$> strTweekday w)
  <$> (query conn selq (crsidTstr c :: Text, n) :: IO [(Text,Int)])
  where selq = "\
    \ SELECT day, period \
    \ FROM class \
    \ WHERE crsid = ? AND sectno = ? \
    \ ;"


getRequir :: Connection -> Crsid -> IO CrsidSet
getRequir conn crs =
  S.fromList . map fromOnly <$> query conn selq (Only crs)
  where selq = "\
    \ SELECT requir \
    \ FROM relation_requir \
    \ WHERE target = ? \
    \ ;"


-- gets Prof information for given name from the server
-- returns Nothing in either case of not existing and not complete
getProf :: Connection -> Text -> IO (Maybe Prof)
getProf conn name = do
  x <- query conn selq (Only name)
  -- there will be one or zero result since crsid is primary
  return $ case fmap nullize3 x of
    [] -> Nothing          -- nothing on database
    [Nothing] -> Nothing   -- database is not coplete
    [Just (off,ful,eml)] -> Just $ Prof name off ful eml
  where
    nullize3 (Just a, Just b, Just c) = Just (a,b,c)
    nullize3 _ = Nothing
    selq = "\
      \ SELECT office, full, email \
      \ FROM professor \
      \ WHERE name = ? \
      \ ;"


-- gets Course (as RefCrs) with given crsid from the server
-- returns Nothing in either case of not existing and not complete
getRefCrs :: Connection -> Crsid -> IO (Maybe RefCrs)
getRefCrs conn c = do
  -- there will be one or zero result since crsid is primary
  x <- query conn selq $ Only (crsidTstr c :: Text)
  return $ case x of
    [] -> Nothing
    -- all fields are not null, we don't check nullity
    [(tlt,tlk,cre)] -> Just . RefCrs $ Course {
      crs_id = c, title = tlt, title_kr = tlk, credit = cre
    }
  where
    selq = "\
      \ SELECT title, title_kr, credit \
      \ FROM course \
      \ WHERE crs_id = ? \
      \ ;"


-- gets Section (as RefSect) with given crsid and sect# from the server
-- returns Nothing in either case of not existing and not complete
getRefSect :: Connection -> (Crsid,Int) -> IO (Maybe RefSect)
getRefSect conn (c,n) = do
  x <- query conn selq (crsidTstr c :: Text, n)
  -- there will be one or zero result since crsid is primary
  case fmap nullize5 x of
    [] -> return Nothing          -- nothing on database
    [Nothing] -> return Nothing   -- database is not coplete
    [Just (prf',t,rom,sz,sme')] -> runMaybeT $ do
      -- FIXME we can do join instead ??
      prf <- MaybeT $ getProf conn prf'
      ltm <- MaybeT $ getLectime conn (c,n)
      sme <- MaybeT . return $ intTsemester sme'
      return . RefSect $ Section {
        crsid = c, sectno = n, prof = prf, ta = TeachAssi t,
        lectime = ltm, roomid = rom, enroll_size = sz, semester = sme
      }
  where
    nullize5 (Just a, Just b, Just c, Just d, Just e) = Just (a,b,c,d,e)
    nullize5 _ = Nothing
    selq = "\
      \ SELECT prof, ta, room, enroll_size, semester \
      \ FROM section \
      \ WHERE crsid = ? AND sect_no = ? \
      \ ;"
