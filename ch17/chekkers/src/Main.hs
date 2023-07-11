{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype ZipList a = ZipList { unZipList :: [a] }
  deriving (Eq, Ord, Show, Arbitrary, EqProp)

{--
-- the one-sided cases could be written as
--   mappend (ZipList x:xs) (ZipList []) =
--     ZipList $ (x `mappend` mempty):...
--     but we have x <> mempty == x from the monoid laws
instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []
  mappend (ZipList []) (ZipList []) = ZipList []
  mappend (ZipList xs) (ZipList []) = ZipList xs
  mappend (ZipList []) (ZipList ys) = ZipList ys
  mappend (ZipList (x:xs)) (ZipList (y:ys)) =
    ZipList $ (x `mappend` y):(unZipList (ZipList (xs `mappend` ys)))
--}

-- another, equivalent, approach:
instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList $ repeat mempty
  mappend (ZipList []) _ = ZipList []
  mappend _ (ZipList []) = ZipList []
  mappend (ZipList (x:xs)) (ZipList (y:ys)) =
    ZipList $ (x `mappend` y):(unZipList (ZipList xs `mappend` ZipList ys))

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

data Validation e a = VError e
                    | VSuccess a
                    deriving (Eq, Show)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof $ [VError <$> arbitrary, VSuccess <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance Functor (Validation e) where
  fmap _ (VError e) = VError e
  fmap f (VSuccess x) = VSuccess (f x)

instance Monoid e => Applicative (Validation e) where
  pure = VSuccess
  VError e1 <*> VError e2 = VError $ e1 <> e2
  VError e <*> VSuccess _ = VError e
  VSuccess f <*> x = fmap f x

newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Eq a => EqProp (Pair a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  -- this seems shady
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two a f <*> Two b x = Two (a <> b) (f x)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three a b f <*> Three c d x = Three (a <> c) (b <> d) (f x)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' a f g <*> Three' b x y = Three' (a <> b) (f x) (g y)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four a1 a2 a3 f <*> Four b1 b2 b3 x = Four (a1 <> b1) (a2 <> b2) (a3 <> b3) (f x)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' a1 a2 a3 f <*> Four' b1 b2 b3 x =
    Four' (a1 <> b1) (a2 <> b2) (a3 <> b3) (f x)

-- orphan instance, but required for Checkers
instance Eq a => EqProp (Sum a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative
    (undefined :: [(Int, Int -> Int, Int -> String)])
  quickBatch $ monoid (undefined :: ZipList (Sum Int))
  quickBatch $ monoid (undefined :: List Int)
  quickBatch $ functor (undefined :: List (Int, Double, String))
  quickBatch $ applicative
    (undefined :: List (Int, String, Double))
  quickBatch $ functor (undefined :: Validation Int (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Validation [String] (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Pair (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Two [Int] (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Three [Int] (Sum Int) (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Three' (Sum Int) (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Four (Sum Int) (Product Int) String (String, Int, Double))
  quickBatch $ applicative
    (undefined :: Four' (Sum Int) (String, Int, Double))
