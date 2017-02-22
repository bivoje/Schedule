{-# LANGUAGE OverloadedStrings #-}

module Schedule.ConsoleIO where

-- import Data.Text as T
--import Data.Foldable (fold)
import Data.List (intersperse)

import Graphics.Vty
import Control.Monad.RWS

import Schedule.Types
import Schedule.Zipper


type TriStat = Maybe Bool

{-
pattern matching to contant is always against
(1) integers or (2) Capital lettered (contructor)
a variable (normal function) cannot be used in
pattern matching, so be careful, do not accidently
use these functions in pattern matching.
use the direct constructors instead (e.g. no -> Just True)
-}

yes :: TriStat
yes = Just True

no :: TriStat
no = Just False

yet :: TriStat
yet = Nothing

type Check a = (a, TriStat)


{-
we could return string to print in list, so that caller
can convert it to image, or do whatever he wants.
(c.f. chrsString :: Check Course -> String)
But, it turned out to be  really hard to cut a string
in title_kr with fixed garaphical width, since full- and
half- width characters are mixed in it.
Vty library functions (crop-seriese) support this like a charm!
-}
-- create Image for check Course to be used in checking list
chcrsImage :: Attr -> Check Course -> Image
chcrsImage attr (crs,s) =
  let cid = text attr $ crsidTstr (crs_id crs)
      tsp = charFill attr ' ' 50 1
      tlt'= text' attr $ title_kr crs
      tlt = cropRight 50 $ tlt' <|> tsp
      cre = string attr $ show (credit crs)
      gab = char attr ' '
      tet = intersperse gab [cid,cre,tlt]
    in mkcheckImage attr s $ horizCat tet


-- TODO we might have to make typeclass for this
-- (converting to image, ToImage, like ToJSON)

-- add checking field to end of given image
mkcheckImage :: Attr -> TriStat -> Image -> Image
mkcheckImage attr s img =
  img <|> text attr (showtr s)
  where showtr s = case s of
          Just True  -> "| y"
          Just False -> "| n"
          Nothing    -> "|  "

-- insnert checking field to start of given image
mkcheckImage' :: Attr -> TriStat -> Image -> Image
mkcheckImage' attr s img =
  text attr (showtr s) <|> img
  where showtr s = case s of
          Just True  -> "y |"
          Just False -> "n |"
          Nothing    -> "  |"
