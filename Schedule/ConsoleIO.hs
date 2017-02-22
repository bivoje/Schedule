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
