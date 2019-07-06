-- {-# LANGUAGE Arrows #-}
-- https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial#Arrow_proc_notation

module Extract where

--import Types

import Codec.Xlsx
import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Control.Lens
import Control.Arrow

--asdf :: Xlsx ->
--    let timetables_openlects = partition (\(t,_) ->
--          "schedule" `isInfixOf` (T.toLower t)
--        ) $ _xlSheets xlsx
--    in map (parseTimetable . snd) ***
--       map (parseOpenlects . snd) $ timetables_openlects

type OpenlectRow =
  ( Maybe Text -- 0 학과
  , Maybe Text -- 1 필수
  , Text       -- 2 id
  , Maybe Text -- 3 과목명
  , Maybe Text -- 4 강실학
  , Maybe Text -- 5 교수
  , Maybe Text -- 6 전임?
  , Maybe Text -- 7 분반
  , Maybe Text -- 8 정원
  , Maybe Text -- 9 신입생
  , Maybe Text -- 10 재학생
  , Maybe Text -- 11 선수과목
  , Maybe Text -- 12 재수강?
  , Maybe Text -- 13 비고
  )

parseOpenlects :: Worksheet
               -> (Int, Int) -- table left-top coord (exclude heading)
               -> (OpenlectRow -> a)
               -> [a]
parseOpenlects ws (r,c) f =
  [ f . tup crsid $ map (flip getCT rx . (+c)) [0..14]
  | (rx, Just crsid) <- zip [r..] . takeWhile isJust . map (getCT $ c+2) $ [r..] ]
  where getCT c r = extractCT <$> ws ^? cellValueAt (r,c) . _Just
        tup x [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o] = (a,b,x,e,f,g,h,i,j,k,l,m,n,o)
        -- TODO block-allocated professor...


type TimetableElem = (Text, Maybe Text, Maybe Text, Maybe Text)
type OnDayTime a = [[a]] -- rectangle 2D table

parseTimetable :: Worksheet
               -> (Int, Int) -- table left-top coord (include heading)
               -> (TimetableElem -> a)
               -> OnDayTime [a]
parseTimetable ws (r,c) f =
  let dws = detectDayWidth ws (r,c+2)
  -- TODO
  -- assert (4, 2+2) == Monday
  -- assert length dws == 5
      tws = detectTimeWidth ws (r+1,c) 4
  in [ [ map f $ onDayTime ws dw tw | tw <- tws ] | dw <- dws ]

-- guess # of columns for each DAY using merged cells
-- (r,c) indicates starting position
--detectDayWidth :: Worksheet -> (Int, Int) -> [Int]
detectDayWidth :: Worksheet -> (Int, Int) -> [(Int,Int)]
detectDayWidth ws (r,c) =
  ws ^. wsMerges
  & map (fromJust . fromRange)
  & filter (and12 <<< (>=c) *** (eq &&& (==r) . fst) <<< snd.fst &&& fst***fst)
  & map (snd *** snd) -- TODO assert no gap?
  & sortOn fst
  -- & map ((+1) . uncurry $ filp (-))
  & map (\(a,b) -> (a, b-a+1))
  where and12 (a,(b,c)) = a && b && c
        eq = uncurry (==)
-- guess # of columns for each Day using cell contents
--detectDayWidth ws (r,c) = detect 5 c
--where
--  detect 0 _ = []
--  detect n s =
--    let len = length $ takeWhile (\i -> isNothing . getCV $ s+i) [1..]
--    in len :: detect (n-1) (len+s+1)
--  getCV c = ws ^? ixCell (r,c) . cellValue . _Just

-- guess # of course-row(consists of [height=4] rows)'s for each lectime
-- (r,c) indicates starting position
detectTimeWidth :: Worksheet -> (Int, Int) -> Int -> [(Int,Int)]
detectTimeWidth ws (r,c) height =
  ws ^. wsMerges
  & map (fromJust . fromRange)
  & filter (and12 <<< (>=r) *** (eq &&& (==c) . fst) <<< fst.fst &&& snd***snd)
  & map (fst *** fst)
  & sortOn fst
  & map (\(a,b) -> (a, flip div height $ b-a+1)) -- TODO assert divisibility
  where and12 (a,(b,c)) = a && b && c
        eq = uncurry (==)

onDayTime :: Worksheet
          -> (Int, Int) -> (Int, Int) -- day widht, time width
          -> [TimetableElem] -- 4 elements
onDayTime ws (ds,dn) (ts,tn) =
  [ (crsid, getCT (r+1) c, getCT (r+2) c, getCT (r+3) c)
  | c <- [ds .. ds + dn - 1],
    r <- [ts, ts+4 .. ts + 4*(tn-1)],
    Just crsid <- [getCT r c] ]
  where getCT r c = extractCT <$> ws ^? cellValueAt (r,c) . _Just


extractCT :: CellValue -> Text
extractCT cv = case cv of
  CellText tttt -> tttt
  CellRich rtrs -> T.concat $ rtrs ^.. traverse . richTextRunText
  CellDouble db -> T.pack $ showdb db
  CellBool bbbb -> T.pack $ if bbbb then "TRUE" else "FALSE"
  CellError err -> T.pack $ "#" ++ show err
  where showdb x | fromIntegral (round x) /= x = show x
                 | otherwise = takeWhile (/='.') $ show x

{-
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
