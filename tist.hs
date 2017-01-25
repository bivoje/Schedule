import Control.Monad
{-
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except

a :: ExceptT String Maybe Integer
a = return 4


ff = do
-}

newtype Popper a b = Popper [b] ([a] -> b) | Insufficient

doPopper :: Popper a b -> [a] -> b
doPopper (Popper [] f) x = f x

instance Functor (Popper a) where
  fmap = liftM

instance Applicative (Popper a) where
  pure x = Popper $ \ls -> x
  (<*>) = ap

instance Monad (Popper a) where
  return = pure
  -- x :: Popper a b
  -- f :: b -> Popper a c
  -- :: Popper a c
  Insufficient >>= _ = Insufficient
  {-
  x >>= f = Popper $ \ls -> case ls of 
    []    -> Insufficient
    (l:s) -> doPopper (f (doPopper x ls)) ls
    -}

--haha = doPopper [1,2,3,4,5] $ do
  
  
do x <- function a
   y <- function b
   return (x,y)

function a >>= (\x -> function b >>= (\y -> return (x,y)))
