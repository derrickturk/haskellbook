module FunctorQc where

import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)

data Possibly a = Nope | Yep a deriving (Eq, Show)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three' x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four' w x y z

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = oneof [return Nope, Yep <$> arbitrary]

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

preservesId :: (Functor f, Eq (f a)) => f a -> Bool
preservesId x = fmap id x == x

type PreservesIdT f a = f a -> Bool

composes :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
composes x (Fun _ f) (Fun _ g) = short == long where
  short = fmap (g . f) x
  long = (fmap g) . (fmap f) $ x

type ComposesT f a b c = f a -> Fun a b -> Fun b c -> Bool

-- end quickcheck boilerplate

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Functor (Four a b c) where
  fmap f (Four w x y z) = Four w x y (f z)

instance Functor (Four' a) where
  fmap f (Four' w x y z) = Four' w x y (f z)

instance Functor Possibly where
  fmap _ Nope = Nope
  fmap f (Yep x) = Yep $ f x

instance Functor (Constant a) where
  fmap _ = Constant . getConstant

main :: IO ()
main = do
  quickCheck (preservesId :: PreservesIdT Identity Int)
  quickCheck (composes :: ComposesT Identity Int String Double)
  quickCheck (preservesId :: PreservesIdT Pair Int)
  quickCheck (composes :: ComposesT Pair Int Double String)
  quickCheck (preservesId :: PreservesIdT Pair Int)
  quickCheck (composes :: ComposesT Pair Int Double String)
  quickCheck (preservesId :: PreservesIdT (Two Double) Int)
  quickCheck (composes :: ComposesT (Two Double) Int Double String)
  quickCheck (preservesId :: PreservesIdT (Three Double String) Int)
  quickCheck (composes :: ComposesT (Three Double String) Int Double String)
  quickCheck (preservesId :: PreservesIdT (Three' Double) Int)
  quickCheck (composes :: ComposesT (Three' Double) Int Double String)
  quickCheck (preservesId :: PreservesIdT (Four Char Char Char) Int)
  quickCheck (composes :: ComposesT (Four Char Char Char) Int Double String)
  quickCheck (preservesId :: PreservesIdT (Four' Char) Int)
  quickCheck (composes :: ComposesT (Four' Char) Int Double String)
  quickCheck (preservesId :: PreservesIdT Possibly Int)
  quickCheck (composes :: ComposesT Possibly Int Double String)
  quickCheck (preservesId :: PreservesIdT (Constant Char) Int)
  quickCheck (composes :: ComposesT (Constant Char) Int Double String)
