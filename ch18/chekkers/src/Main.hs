{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid ((<>))

data Sum a b = First a 
             | Second b
             deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  Second f <*> x = fmap f x

instance Monad (Sum a) where
  First x >>= _ = First x
  Second x >>= f = f x

data Nope a = Nuffin deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = return Nuffin

instance Functor Nope where
  fmap _ _ = Nuffin

instance Applicative Nope where
  pure _ = Nuffin
  _ <*> _ = Nuffin

instance Monad Nope where
  _ >>= _ = Nuffin

instance EqProp (Nope a) where (=-=) = eq

data BackEither b a = Frist a 
                    | Scond b
                    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (BackEither a b) where
  arbitrary = oneof [Frist <$> arbitrary, Scond <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BackEither a b) where
  (=-=) = eq

instance Functor (BackEither b) where
  fmap f (Frist x) = Frist $ f x
  fmap _ (Scond x) = Scond x

instance Applicative (BackEither a) where
  pure = Frist
  Frist f <*> x = fmap f x
  Scond x <*> _ = Scond x

instance Monad (BackEither a) where
  Frist x >>= f = f x
  Scond x >>= _ = Scond x

newtype Identity a = Identity a deriving (Eq, Ord, Show, Arbitrary, EqProp)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  Identity x >>= f = f x

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = oneof [return Nil, Cons <$> arbitrary <*> arbitrary]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Monoid (List a) where
  mempty = Nil
  mappend Nil ys = ys
  mappend (Cons x xs) ys = Cons x (mappend xs ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure = flip Cons Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = (f <$> xs) <> (fs <*> xs)

-- List a -> (a -> List b) -> List b

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons x xs) = x `mappend` concatList xs

instance Monad List where
  xs >>= f = concatList $ fmap f xs

main :: IO ()
main = do
  quickBatch $ functor (undefined :: Sum String (Int, Double, String))
  quickBatch $ applicative (undefined :: Sum String (Int, Double, String))
  quickBatch $ monad (undefined :: Sum String (Int, Double, String))
  quickBatch $ functor (undefined :: Nope (Int, Double, String))
  quickBatch $ applicative (undefined :: Nope (Int, Double, String))
  quickBatch $ monad (undefined :: Nope (Int, Double, String))
  quickBatch $ functor (undefined :: BackEither String (Int, Double, String))
  quickBatch $ applicative (undefined :: BackEither String (Int, Double, String))
  quickBatch $ monad (undefined :: BackEither String (Int, Double, String))
  quickBatch $ functor (undefined :: Identity (Int, Double, String))
  quickBatch $ applicative (undefined :: Identity (Int, Double, String))
  quickBatch $ monad (undefined :: Identity (Int, Double, String))
  quickBatch $ monad (undefined :: List (Int, Double, String))
