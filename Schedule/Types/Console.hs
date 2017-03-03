{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Schedule.Types.Console where

import Data.List (intersperse)
import Data.Maybe (mapMaybe)

import Graphics.Vty
import Graphics.Vty.Image.Internal (Image(BGFill))

import Schedule.Types
import Schedule.Types.Zipper


-- these two functions are modified version of vty library functions

-- | Resize the width. Pads and crops as required to assure the given
-- display width. This is biased to pad/crop on the right.
resizeWidth' :: Int -> Image -> Image
resizeWidth' w i = case w `compare` imageWidth i of
    LT -> cropLeft w i
    EQ -> i
    GT -> BGFill (w - imageWidth i) (imageHeight i) <|> i

-- | Resize the height. Pads and crops as required to assure the given
-- display height. This is biased to pad/crop on the bottom.
resizeHeight' :: Int -> Image -> Image
resizeHeight' h i = case h `compare` imageHeight i of
    LT -> cropTop h i
    EQ -> i
    GT -> BGFill (imageWidth i) (h - imageHeight i) <-> i


-- default Hightlight attribute
defHiAttr = Attr { attrStyle = Default
                 , attrForeColor = SetTo black
                 , attrBackColor = SetTo white
                 }


-- types that can be converted to vty images
class ToImage a where
  imagine :: Attr -> a -> Image

instance ToImage String where
  imagine = string

instance ToImage Course where
  imagine attr crs =
    let cid = text attr $ crsidTstr (crs_id crs)
        tlt'= text' attr $ title_kr crs
        tlt = resizeWidth 50 $ tlt'
        cre = string attr $ show (credit crs)
        gab = char attr ' '
        tet = intersperse gab [cid,cre,tlt]
     in horizCat tet

instance ToImage a => ToImage (Zipper a) where
  imagine attr z = if isEmpty z then string attr "\n" else
    let (abv,m:blw) = toPair z
     in foldMap (imagine attr) (reverse abv)
      <-> imagine defHiAttr m
      <-> foldMap (imagine attr) blw


-- Maybe Bool replacement for the pattern matching comfort
data TriStat = Yes | No | Yet
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- add following state field to given image
-- "field1 field2" :> "field1 field2 | y"
stateImage :: Attr -> TriStat -> Image -> Image
stateImage attr s img =
  img <|> text attr (showtr s)
  where showtr s = case s of
          Yes -> "| y"
          No  -> "| n"
          Yet -> "|  "

-- add leading state field to given image
-- "field1 field2" :> "y | field1 field2"
stateImage' :: Attr -> TriStat -> Image -> Image
stateImage' attr s img =
  text attr (showtr s) <|> img
  where showtr s = case s of
          Yes -> "y |"
          No  -> "n |"
          Yet -> "  |"


-- wrapper that adds attribute of tristate to 'a'
data Check a = Check a TriStat

instance ToImage a => ToImage (Check a) where
  imagine attr (Check a s) =
    mkcheckImage attr s $ imagine attr a

runCheck :: Check a -> Maybe a
runCheck (Check a Yes) = Just a
runCheck (Check a _) = Nothing

isYes :: Check a -> Bool
isYes (Check _ Yes) = True
isYes _ = False

isNo :: Check a -> Bool
isNo (Check _ No) = True
isNo _ = False

isYet :: Check a -> Bool
isYet (Check _ Yet) = True
isYet _ = False

toggleCheck :: Check a -> Check a
toggleCheck (Check a Yes) = Check a No
toggleCheck (Check a No ) = Check a Yet
toggleCheck (Check a Yet) = Check a Yes


-- check-list + list-zipper :> checkzipper
type CheckZipper a = Zipper (Check a)

listTcz :: [a] -> CheckZipper a
listTcz = fromList . map (flip Check . Yet)

czTlist :: CheckZipper a -> [a]
czTlist = mapMaybe runCheck . toList


-- windowed zipper
newtype ZipperW a = ZipperW (Int, Zipper a)

-- behavior of imagine ZipperW is differenct from that of imagine Zipper
-- with that ZipperW would pad the lines of there isn't sufficient
-- elements in it, c.f. image's heigt of imagine ZippweW is always 2n+1
-- while imagine Zipper would vary
instance ToImage a => ToImage (ZipperW a) where
  imagine attr (ZipperW (n,z)) =
    let (abv',blw') = toPair $ windowz n z
        abv = foldMap (imagine attr) (reverse abv')
        (m,blw) = images blw'
        abvi = resizeHeight' n abv
        blwi = resizeHeight n blw
     in abvi <-> m <-> blwi
    where images [] = (emptyImage, emptyImage)
          images (b:bs) = (imagine defHiAttr b, foldMap (imagine attr) bs)
