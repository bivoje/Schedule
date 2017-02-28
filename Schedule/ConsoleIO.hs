module Schedule.ConsoleIO where

import Control.Monad.RWS

import Graphics.Vty

import Schedule.Types.Zipper
import Schedule.Types.Console


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
