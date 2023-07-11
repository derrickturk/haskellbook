module Ch20 where

import Data.Monoid((<>), Sum(..), Product(..), Any(..), All(..))

data Identity a = Identity a deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Some a | None deriving (Eq, Show)

instance Foldable Optional where
  foldr _ z None = z
  foldr f z (Some x) = f x z

  foldMap _ None = mempty
  foldMap f (Some x) = f x

-- exercises:

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (Any . (== x))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr newMin Nothing where
  newMin x Nothing = Just x
  newMin x (Just min)
    | x < min = Just x
    | otherwise = Just min

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr newMax Nothing where
  newMax x Nothing = Just x
  newMax x (Just max)
    | x > max = Just x
    | otherwise = Just max

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> True) False

length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (Sum . const 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z

data Two a b = Two a b deriving Show

instance Foldable (Two a) where
  foldr f z (Two _ x) = f x z

data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ x y) = f x $ f y z

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ w x y) = f w $ f x $ f y z

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
           (a -> Bool) -> t a -> f a
filterF f xs = foldMap (\x -> if f x then pure x else mempty) xs 
