module Semigroups where

import Data.Semigroup

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

{--
instance Arbitrary Trivial where
  arbitrary = return Trivial
--}

type AssocType a = a -> a -> a -> Bool

semigroupAssoc :: (Eq a, Semigroup a) => AssocType a
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity $ x <> y

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two c d = Two (a <> c) (b <> d)

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Three a b c) where
  Three a b c <> Three d e f = Three (a <> d) (b <> e) (c <> f)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
    Semigroup (Four a b c d) where
  Four a b c d <> Four e f g h = Four (a <> e) (b <> f) (c <> g) (d <> h)

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj x) <> (BoolConj y) = BoolConj $ x && y

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj x) <> (BoolDisj y) = BoolDisj $ x || y

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  Fst _ <> x = x

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ \x -> f x <> g x

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  f <> g = Comp $ unComp f . unComp g

-- continued in monoidQc because fuck
