module Schedule.ConsoleIO where

import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.RWS

import Graphics.Vty

import Schedule.Types
import Schedule.Types.Zipper
import Schedule.Types.Console



-------------------------------------------------------------------------
-- picker


type Pick a = RWST Vty () (CheckZipper a) IO


pickFrom :: ToImage a => [a] -> IO [a]
pickFrom ls = do
  vty <- mkVty defaultConfig
  (s,_) <- execRWST pickProcess vty (listTcz ls)
  shutdown vty
  return $ czTlist s


pickProcess :: ToImage a => Pick a ()
pickProcess = do
  viewCheckZipper
  e <- ask >>= liftIO . nextEvent
  b <- pickKeyHandle e
  when b pickProcess


pickKeyHandle :: Event -> Pick a Bool
pickKeyHandle (EvKey (KChar 'q') []) = return False
pickKeyHandle k = case k of
  EvKey KUp         [] -> modify left
  EvKey KDown       [] -> modify right
  EvKey (KChar ' ') [] -> modify ch
  _              -> return ()
 >> return True
  where ch = touch toggleCheck


viewCheckZipper :: ToImage a => Pick a ()
viewCheckZipper = do
   pic <- gets $ picForImage . imagine defAttr . ZipperW . (,) 10
   ask >>= liftIO . flip update pic



-------------------------------------------------------------------------
-- Timetable Render


renderTimetable = renderTimetable' ('│','─','┼',' ',8,3)

-- assumes ttm is proper matrix
renderTimetable' :: (Char,Char,Char,Char,Int,Int)
                 -> [[Maybe RefSect]] -> Text
renderTimetable' (horzd,vertd,cross,sps,celw,nu) ttm' =
  let heading' = map (T.center celw sps . weekdayTstr) [Monday .. Friday]
      heading = rep nu sps `T.append` rowlize horzd heading'
      ttm = transpose ttm'
      content = map (rowlize horzd . map rRefSect) ttm
      horline' = replicate 5 $ T.replicate celw $ T.singleton vertd
      horline = rep nu vertd `T.append` rowlize cross horline'
      label = map (T.justifyRight nu sps . T.pack . show) [1..]
   in T.unlines $ heading : horline : zipWith mappend label content
  where rep n = T.replicate n . T.singleton
        rRefSect Nothing = T.replicate celw (T.singleton sps)
        rRefSect (Just x) = refsectTstr x
        rowlize d ls =
          horzd `T.cons` T.intercalate (T.singleton d) ls `T.snoc` horzd
