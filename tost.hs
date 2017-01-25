newtype EIO a = EIO (Either String (IO a))

print' :: Show a => EIO a -> IO ()
print' (EIO a) =
  case a of Left str -> putStrLn $ "EIO " ++ str
            Right iox -> iox >>= (\x -> putStrLn $ "EIO " ++ show x)

a :: EIO Integer
a = EIO $ Right $ return 3

f :: Integer -> Float
f = fromInteger

iof :: IO (Integer -> Float)
iof = return f

eiof :: Either String (IO (Integer -> Float))
eiof = return iof

eiof' :: Either String (IO (Integer -> Float))
eiof' = Left "erorr"

_eiof :: EIO (Integer -> Float)
_eiof = EIO eiof

_eiof' :: EIO (Integer -> Float)
_eiof' = EIO eiof'

instance Functor EIO where
  fmap f (EIO x) = EIO $ fmap (fmap f) x

instance Applicative EIO where
  pure = EIO . Right . return
  x@(EIO (Left _) <*> eioa = x
  (EIO (Right iof) <*> eioa =
    deduce $ iof >>= (\f -> fmap f iob)
    where deduce 
  {-
  iof >>= (\f ->
    case fmap f eioa of x@(EIO (Left _)) -> x
                        (EIO (Right iob) -> 
                        -}
