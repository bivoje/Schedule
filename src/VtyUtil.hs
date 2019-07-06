
module VtyUtil where


import Graphics.Vty
import Control.Monad.RWS


-- from utility-ht Data.Ord.HT
{-# INLINE limit #-}
limit :: (Ord a) => (a,a) -> a -> a
limit (l,u) = max l . min u


selectList :: (Attr -> a -> Image)
           -> Int -> [a] -> a
           -> Vty -> IO (Maybe Int)
selectList f idx elems title vty = do
  liftIO draw
  e <- nextEvent vty
  let range = (0, length elems - 1) in case e of
    EvKey KEsc   _ -> return Nothing
    EvKey KUp    _ -> recurse $ limit range (idx-1)
    EvKey KDown  _ -> recurse $ limit range (idx+1)
    EvKey KEnter _ -> return $ Just idx
    _ -> recurse idx
  where
    recurse idx = selectList f idx elems title vty
    draw =
      let heading = f defAttr title
          body = pad 4 0 0 0 . vertCat $ zipWith formatElem [0..] elems
       in update vty $ picForImage (heading <-> body)
    formatElem n elem
      | n ==  idx = f (defAttr `withStyle` reverseVideo) elem
      | otherwise = f defAttr elem
