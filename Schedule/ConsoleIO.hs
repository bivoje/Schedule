module Schedule.ConsoleIO where

import Control.Monad.RWS

import Graphics.Vty

import Schedule.Types.Zipper
import Schedule.Types.Console


type Pick a = RWST Vty () (CheckZipper a) IO

viewCheckZipper :: ToImage a => Pick a ()
viewCheckZipper = do
   pic <- gets $ picForImage . imagine defAttr . ZipperW . (,) 10
   ask >>= liftIO . flip update pic
