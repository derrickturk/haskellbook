module Ch22 where

import Control.Applicative
import Data.Maybe

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  pure = Reader . const
  f <*> x = Reader $ \r -> runReader f r $ runReader x r

instance Monad (Reader r) where
  m >>= k = Reader $ \r -> runReader (k $ runReader m r) r

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks = Reader

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

xs = lookup 3 $ zip x y
ys = lookup 6 $ zip y z
zs = lookup 4 $ zip x y
z' = flip lookup $ zip x z

x1 = liftA2 (,) xs ys
x2 = liftA2 (,) ys zs
x3 n = liftA2 (,) (z' n) (z' n)

summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA =  sequenceA [(> 3), (< 8), even]

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  -- why?
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> ys)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7
  print $ foldr (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ sequA $ fromMaybe 0 ys
