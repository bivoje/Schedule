{-# LANGUAGE OverloadedStrings #-}

module Explorer where


import Codec.Xlsx
import Graphics.Vty

import Data.Text (Text)
import qualified Data.Text as T
import Data.List
import Data.Maybe (fromMaybe)

import Control.Monad.RWS
import Control.Lens


-- from utility-ht Data.Ord.HT
{-# INLINE limit #-}
limit :: (Ord a) => (a,a) -> a -> a
limit (l,u) = max l . min u


type Operate = RWST Vty () Xlsx IO

explorer :: Operate ()
explorer = do
  mtitle <- selectSheets 0
  case mtitle of
    Nothing -> return ()
    Just title -> explorerSheet title (1, 1) >> explorer

selectSheets :: Int -> Operate (Maybe Text)
selectSheets idx = do
  selectSheetsScreen idx -- FIXME drawn too many times ...
  n <- gets (length . _xlSheets)
  e <- asks nextEvent >>= liftIO
  case e of
    EvKey KEsc   _ -> return Nothing
    EvKey KUp    _ -> selectSheets $ limit (0, n) (idx-1)
    EvKey KDown  _ -> selectSheets $ limit (0, n) (idx+1)
    EvKey KEnter _ -> gets $ Just . fst . (!! idx) . _xlSheets
    _ -> selectSheets idx

-- TODO we don't have to [formatTitle] at every time...
selectSheetsScreen :: Int -> Operate ()
selectSheetsScreen idx = do
  let heading = string defAttr "select one among following sheets in xlsx file..." 
  titles <- gets (map fst . _xlSheets)
  let body = pad 4 0 0 0 . vertCat $ zipWith formatTitle [0..] titles
  vty <- ask
  liftIO $ update vty $ picForImage (heading <-> body)
  where formatTitle n title
          | n ==  idx = text' (defAttr `withStyle` reverseVideo) title
          | otherwise = text' defAttr title

explorerSheet :: Text -> (Int, Int) -> Operate ()
explorerSheet title pos@(r, c) = do
  explorerSheetScreen title pos
  e <- asks nextEvent >>= liftIO
  case e of
    EvKey KEsc   _ -> return ()
    EvKey KRight _ -> explorerSheet title (r, c+1)
    EvKey KLeft  m | MCtrl `elem` m -> explorerSheet title (r, 1)
    EvKey KLeft  m | otherwise      -> explorerSheet title (r, max 1 (c-1))
    EvKey KUp    m | MCtrl `elem` m -> explorerSheet title (1, c)
    EvKey KUp    m | otherwise      -> explorerSheet title (max 1 (r-1), c) 
    EvKey KDown  _ -> explorerSheet title (r+1, c)
    --EvKey (KChar 'g') _ ??? -> >>= explorerSheet title
    _ -> explorerSheet title pos
  where

explorerSheetScreen :: Text -> (Int, Int) -> Operate ()
explorerSheetScreen title pos@(r, c) = do
  let header = string defAttr $ "at (" ++ show r ++ ", " ++ show c ++ ")"
  xlsx <- get
  let body = cellsImage 10 xlsx title pos
  let mcv = xlsx ^? ixSheet title . ixCellRC pos . cellValue . _Just
  let footer = showCellValue 80 $ fromMaybe (CellText "EMPTY") mcv
  let img = header <-> translate 2 0 body <-> translate 0 1 footer
  asks (flip update . picForImage $ img) >>= liftIO

cellsImage :: Int -> Xlsx -> Text -> (Int, Int) -> Image
cellsImage width xlsx title (r, c) =
  let ls = for [ r+rd | rd <- [-1, 0, 1] ] $ \rx ->
        let ls = for [ c+cd | cd <- [-1, 0, 1] ] $ \cx ->
              let mcv = xlsx ^? ixSheet title . ixCellRC (rx, cx) . cellValue . _Just
              in  showCellValue width $ fromMaybe (CellText T.empty) mcv
        in let vertsep = char (defAttr `withStyle` bold) '|'
        in horizCat $ intersperse vertsep ls
  in let horisep' = charFill (defAttr `withStyle` bold) '-' width 1
  in let crossep = char (defAttr `withStyle` bold) '+'
  in let horisep = horizCat . intersperse crossep $ replicate 3 horisep'
  in vertCat $ intersperse horisep ls
  where for = flip map

showCellValue :: Int -> CellValue -> Image
showCellValue width cv = resizeWidth width $ imaginary cv
  where imaginary (CellText tttt) = text' defAttr $ T.map newline2space tttt
        imaginary (CellDouble db) = string (defAttr `withForeColor` green)$ show db
        imaginary (CellBool bbbb) = string (defAttr `withForeColor` blue) $ show bbbb
        imaginary (CellRich rctr) = string (defAttr `withForeColor` red)  $ "RICHTEXT"
        imaginary (CellError err) = string (defAttr `withBackColor` red)  $ show err
        newline2space '\n' = ' '
        newline2space '\r' = ' '
        newline2space c = c
