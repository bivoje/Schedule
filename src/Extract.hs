
module Extract where

--import Types

import Codec.Xlsx
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List
import Control.Lens
import Control.Arrow

{-
bs <- L.readFile "test/schedule.xlsx"
case toXlsxEither bs of
  Right _ -> ?
  Left xlsx ->
    let timetables_openlects = partition (\(t,_) ->
          "schedule" `isInfixOf` (T.toLower t)
        ) $ _xlSheets xlsx
    in map (parseTimetable . snd) ***
       map (parseOpenlects . snd) $ timetables_openlects

parseTimetable :: Worksheet -> ???
parseTimetable sheets

-- convert to string as saved in db
-- e.g. (GS,101) -> "GS0101"
-- crsid assumed to have valid values
crsidTstr :: IString s => Crsid -> s
crsidTstr (Crsid mj n) = assert (0 <= n && n < 10000) $
  schoolTstr mj <> fromString (swrap n)
  where swrap = reverse . take 4 . reverse . (zeros ++) . show
        zeros = replicate 4 '0'

-- parses the school data from db
-- e.g. (GS,1101) -> "GS1101"
strTcrsid :: String -> Maybe Crsid
strTcrsid s =
  let (mj,n) = splitAt 2 s
  in Crsid <$> strTschool sc
           <*> f n
  where -- efficiently (lazily?) check if the length is 4
    f s@[_,_,_,_] | all isNumber s = Just (read s)
                  | otherwise = Nothing

sectidTstr :: IString s => Sectid -> s
sectidTstr (Sectid cs n) =
  crsidTstr cs <> "-" <> fromString (show n)

strTsectid :: String -> Maybe Sectid
strTsectid s =
  let (cs,n) = splitAt 6 s
  in Sectid <$> strTcrsid cs
            <*> f n
  where f s | all isNumber s = Just (read s)
            | otherwise = Nothing

-}
