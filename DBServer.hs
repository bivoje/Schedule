{-# LANGUAGE OverloadedStrings #-}

module DBServer where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Set as S (fromList)
import Database.MySQL.Simple
import System.IO
import Control.Exception (bracket_)

import Types.Internal
{- DBServer module imports Types.Internal directly
 - and provides getter-functions of some composite types.
 - Using those functions would guarantee the validity of the data.
 -}


-- gets Course (as RefCrs) with given crsid from the server
getRefCrs :: Connection -> Crsid -> IO (Maybe RefCrs)
getRefCrs conn c = do
  -- there will be one or zero result since crsid is primary
  x <- query conn selq $ Only (crsidTstring c :: Text)
  case x of
    [] -> return Nothing
    [(tlt,tlk,cre,rq1,rq2,rq3)] -> return . Just . RefCrs $ Course {
      crs_id = c, title = tlt, title_kr = tlk, credit = cre,
      requir = S.fromList . mapMaybe (>>=stringTcrsid) $ [rq1,rq2,rq3]
    }
  -- we have crsid already, so don't query it again
  where
    selq = "\
      \ SELECT title, title_kr, credit, requir1, requir2, requir3 \
      \ FROM course \
      \ WHERE crs_id = ? \
      \ ;"
