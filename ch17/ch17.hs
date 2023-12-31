module Ch17 where

import Data.List (elemIndex)
import Control.Applicative (liftA3)

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x' :: Maybe Int
x' = elemIndex 3 [1..5]

y' :: Maybe Int
y' = elemIndex 4 [1..5]

maxed :: Maybe Int
maxed = max <$> x' <*> y'

x'' :: Maybe Integer
x'' = lookup 3 $ zip [1..3] [4..6]

y'' :: Maybe Integer
y'' = lookup 2 $ zip [1..3] [4..6]

-- this is a dumb function given what sum does on tuples
summed :: Maybe Integer
summed = fmap sum $ (,) <$> x'' <*> y''

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  -- fmap = id | nope, remember we have a phantom b
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant f <*> Constant x = Constant $ f `mappend` x

stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
