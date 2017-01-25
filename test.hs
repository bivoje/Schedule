import Control.Monad

newtype IOE a = IOE (IO (Either String a))

getIOE :: IOE a -> IO (Either String a)
getIOE (IOE a) = a

getRight :: Either String a -> a
getRight (Right a) = a

print' (IOE a) = a >>= \b -> putStrLn $ "IOE (" ++ show b ++ ")"

instance Functor IOE where
  --fmap :: (a -> b) -> IOE a -> IOE b
  fmap f (IOE a) = IOE $ fmap (fmap f) a

{-
instance Applicative IOE where
  --pure :: a -> IOE a
  pure = IOE . return . Right
  --(<*>) :: IOE (a -> b) -> IOE a -> IOE b
  (IOE ioef) <*> a = ioef >>= (\ef -> ef >>= (\f -> fmap f a))

instance Monad IOE where
  (IOE x) >>= f = fmap join $ x
    >>= either (IOE . return . Left) (fmap Right . f)
-}
--convert1 :: IO (Either String (IO a)) -> IO (Either String a)
--convert1 x = x >>= either (return . Left) (fmap Right)

convert1 :: IO (Either String a) -> (a -> IO b) -> IO (Either String b)
convert1 x f = x >>= either (return . Left) (fmap Right . f)

convert2 :: IO (Either String a)
         -> (a -> IO (Either String b))
         -> IO (Either String b)
convert2 x f = fmap join $ x >>= either (return . Left) (fmap Right . f)

ff :: Int -> IO (Either String Float)
ff a = return $ if a < 0 then Left "error" else Right (fromIntegral a + 0.5)

a :: IO (Either String Int)
a = return $ Left "orror"

b :: IO (Either String Int)
b = return $ Right 3

c :: IO (Either String Int)
c = return $ Right (-3)
