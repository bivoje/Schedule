{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Char
import Control.Monad.Trans.Except

import Parser

--showTblColumn :: (a -> String) -> [[a]] -> String
prettyTblSel f tbl =
  let ftbl = fixtbl tbl
      n = length $ head ftbl
      irow = map show $ take n [1..]
      cols = transpose $ irow : map (map f) ftbl
      fcols = map fixwidth cols
      frows = transpose fcols
      (sirow : srows) = map (intercalate "|") frows
      sep = replicate (graphic_length sirow) '-'
   in (n,intercalate "\n" $ sirow : sep : srows)
      

  where
  graphic_length [] = 0
  graphic_length (s:tr) =
    graphic_length tr + uni_width s

  uni_width c = 
    if isAscii c then 1 else
      if c == 'Ⅱ' then 1 else 2
      
  fixtbl tbl = 
    let colnum = minimum $ map length tbl
     in map (take colnum) tbl
    
  fixwidth col =
    let width = maximum $ map graphic_length col
     in map (middle width) col

  middle width str =
    let dmarg = width - graphic_length str + 2
        lmarg = dmarg `div` 2
        rmarg = dmarg - dmarg `div` 2
     in replicate lmarg ' ' ++ str ++ replicate rmarg ' '

updateManip file = do 
  tbl <- ExceptT $ parse_openlects file
  --let (n,ptbl) = prettyTblSel f . take 3 . tail $ tbl
  return $ prettyTblSel f . take 3 . tail $ tbl
  --in putStrLn ptbl >> return n
  where 
    f = takeWhile (\c -> c/='\n' && c/='|' && c/='\t' && not (isControl c))

{-
import Database.MySQL.Simple


mydefaultConnectInfo =
  defaultConnectInfo { connectPassword = "sql"
                     , connectDatabase = "classicmodels"
                     }

readExact :: String -> String -> Maybe String
readExact cmp str = if cmp == str then Just str else Nothing

readTripleColon :: (Read a, Read b, Read c) => String -> Maybe (a,b,c)
readTripleColon sta =
  case reads sta of
    [(a,':':stb)] -> case reads stb of
      [(b,':':stc)] -> case reads stc of
        [(c,_)] -> Just (a,b,c)
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing
      
haha :: [String] -> ??
haha strs = do
  (a,as) <- uncons strs
  sch <- readMaybe a :: Maybe String
  (b,bs) <- uncons as
  cls <- readMaybe b :: Maybe String
  (c,cs) <- uncons bs
  crn <- readMaybe c :: Maybe String
  (d,ds) <- uncons cs
  (crtkr,crt_) <- span (/='\n') ds
  crt <- uncons crt_ -- removing leading newline of crt_
  (e,es) <- uncons ds
  (_,_,crd) <- readTripleColon es :: Maybe (Int,Int,Int)
  (f,fs) <- uncons es
  prf <- readMaybe f :: Maybe String
  (g,gs) <- uncons fs
  fll <- fmap (=="전임") $ readMaybe g :: Maybe String
  (h,hs) <- uncons gs
  sct <- if null h then return 1 else readMaybe h :: Maybe Int
  (i,is) <- uncons hs
  mxe <- readmaybe i :: Maybe Int

  

insertToSql lss =
  let do
  c <- connect mydefaultConnectInfo
  xs <- query_ c "SHOW DATABASES"

insertOpenlects :: String -> ExceptT String ())
insertOpenlects file =
     
main = do
  c <- connect mydefaultConnectInfo
  xs <- query_ c "SHOW DATABASES"
  mapM_ (\(Only x) -> putStrLn x) (xs :: [Only String])
  close c

-}
