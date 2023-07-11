{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid ((<>))
import Control.Applicative (liftA2, liftA3)

newtype Identity a = Identity a deriving (Eq, Ord, Show, Arbitrary, EqProp)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity $ f x

instance Monad Identity where
  Identity x >>= f = f x

instance Foldable Identity where
  foldr f z (Identity x) = f x z

-- pull out the Applicative, inject our Identity constructor into it
instance Traversable Identity where
  sequenceA (Identity x) = Identity <$> x
  -- for S&G
  traverse f (Identity x) = Identity <$> f x

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

instance Foldable List where
  foldr _ z Nil = z
  foldr f z (Cons x xs) = f x (foldr f z xs)

instance Traversable List where
  -- sequenceA Nil = pure Nil
  -- sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs
  -- or:
  sequenceA = foldr (liftA2 Cons) (pure Nil)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three c d x = Three (a <> c) (b <> d) (f x)

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

instance Traversable (Three a b) where
  sequenceA (Three x y z) = (Three x y) <$> z

data Pair a b = Pair a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Eq a, Eq b) => EqProp (Pair a b) where (=-=) = eq

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure = Pair mempty
  -- this seems shady
  (Pair m1 f) <*> (Pair m2 x) = Pair (m1 <> m2) (f x)

instance Foldable (Pair a) where
  foldr f z (Pair _ x) = f x z

instance Traversable (Pair a) where
  sequenceA (Pair x y) = (Pair x) <$> y

data Big a b = Big a b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Big x y z

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

-- not actually required for Foldable or Traversable
instance Monoid a => Applicative (Big a) where
  pure x = Big mempty x x
  (Big m1 f g) <*> (Big m2 x y) = Big (m1 <> m2) (f x) (g y)

instance Foldable (Big a) where
  foldr f z (Big _ x y) = f x $ f y z

-- just like a list, use the Applicative instance
--   (for the contents, not Big a) to combine the
--   individual contents
instance Traversable (Big a) where
  sequenceA (Big x a1 a2) = liftA2 (Big x) a1 a2

data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Bigger w x y z

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

-- not actually required for Foldable or Traversable
instance Monoid a => Applicative (Bigger a) where
  pure x = Bigger mempty x x x
  (Bigger m1 f g h) <*> (Bigger m2 x y z) = Bigger (m1 <> m2) (f x) (g y) (h z)

instance Foldable (Bigger a) where
  foldr f z (Bigger _ w x y) = f w $ f x $ f y z

-- just like a list, use the Applicative instance
--   (for the contents, not Bigger a) to combine the
--   individual contents
instance Traversable (Bigger a) where
  sequenceA (Bigger x a1 a2 a3) = liftA3 (Bigger x) a1 a2 a3

data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- the books EqProp instance fucks you hard:
--   aka it doesn't work
--   and will cause tests to lie about perfectly good instances
instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S xs x) = S (fmap f xs) (f x)

instance Foldable n => Foldable (S n) where
  foldr f z (S xs x) = f x (foldr f z xs)

instance Traversable n => Traversable (S n) where
  sequenceA (S as a) = (flip S) <$> a <*> sequenceA as
  -- my first bad try was:
  -- sequenceA (S as a) = S <$> sequenceA as <*> a,
  --   but this breaks the ordering (compare foldr above)

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

-- fuck this gay earth
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    x <- arbitrary
    l <- arbitrary
    r <- arbitrary
    frequency [(1, return Empty),
               (1, return (Leaf x)),
               (1, return (Node l x r))]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- these instances do preorder traversal, because I had ASTs in mind
--   you could write newtypes for inorder or postorder traversal

instance Foldable Tree where
  foldr _ z Empty = z
  foldr f z (Leaf x) = f x z
  foldr f z (Node l x r) = f x $ foldr f (foldr f z r) l

  -- for s&g
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = f x `mappend` foldMap f l `mappend` foldMap f r

instance Traversable Tree where 
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = Leaf <$> a
  sequenceA (Node l a r) = flip Node <$> a <*> sequenceA l <*> sequenceA r

  -- for s&g
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) = flip Node <$> f x <*> traverse f l <*> traverse f r

type Trigger a = a (Int, Int, [Int])

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Trigger Identity)
  quickBatch $ traversable (undefined :: Trigger List)
  quickBatch $ traversable (undefined :: Trigger (Three Int String))
  quickBatch $ traversable (undefined :: Trigger (Pair String))
  quickBatch $ applicative (undefined :: Trigger (Big String))
  quickBatch $ traversable (undefined :: Trigger (Big String))
  quickBatch $ applicative (undefined :: Trigger (Bigger String))
  quickBatch $ traversable (undefined :: Trigger (Bigger String))
  quickBatch $ functor (undefined :: Trigger (S []))
  quickBatch $ traversable (undefined :: Trigger (S []))
  quickBatch $ traversable (undefined :: Trigger Tree)
