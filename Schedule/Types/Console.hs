{-# LANGUAGE OverloadedStrings #-}

module Schedule.Types.Console where

import Data.List (intersperse)
import Graphics.Vty

import Schedule.Types


class ToImage a where
  imagine :: Attr -> a -> Image


instance ToImage Course where
  imagine attr crs = 
    let cid = text attr $ crsidTstr (crs_id crs)
        tsp = charFill attr ' ' 50 1
        tlt'= text' attr $ title_kr crs
        tlt = cropRight 50 $ tlt' <|> tsp
        cre = string attr $ show (credit crs)
        gab = char attr ' '
        tet = intersperse gab [cid,cre,tlt]
     in horizCat tet


data TriStat = Yes | No | Yet
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- add checking field to end of given image
mkcheckImage :: Attr -> TriStat -> Image -> Image
mkcheckImage attr s img =
  img <|> text attr (showtr s)
  where showtr s = case s of
          Yes -> "| y"
          No  -> "| n"
          Yet -> "|  "

-- insnert checking field to start of given image
mkcheckImage' :: Attr -> TriStat -> Image -> Image
mkcheckImage' attr s img =
  text attr (showtr s) <|> img
  where showtr s = case s of
          Yes -> "y |"
          No  -> "n |"
          Yet -> "  |"


newtype Check a = Check (a, TriStat)

instance ToImage a => ToImage (Check a) where
  imagine attr (Check (a,s)) =
    mkcheckImage attr s $ imagine attr a
