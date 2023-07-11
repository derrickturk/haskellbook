module Lib where

import Data.Semigroup

data Optional a = Nada | Only a deriving (Eq, Show)

-- "real" Monoid, fake Maybe
instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend x y = Only $ project x `mappend` project y where
    project Nada = mempty
    project (Only x) = x

-- another Maybe Monoid
newtype Firstish a = Firstish { getFirstish :: Optional a }
  deriving (Eq, Show)

instance Monoid (Firstish a) where
  mempty = Firstish Nada
  mappend (Firstish Nada) (Firstish Nada) = Firstish Nada
  mappend fx@(Firstish (Only x)) _ = fx
  mappend (Firstish Nada) fy@(Firstish (Only y)) = fy

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

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

data Validation a b = Faylure a | Sucksess b deriving (Eq, Show)

-- an either <> that take the first failure or the last success
instance Semigroup (Validation a b) where
  Faylure x <> _ = Faylure x
  _ <> Faylure x = Faylure x
  _ <> Sucksess x = Sucksess x

newtype AccumulateSuccess a b = AccumulateSuccess (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateSuccess a b) where
  AccumulateSuccess (Faylure x) <> _ = AccumulateSuccess (Faylure x)
  _ <> AccumulateSuccess (Faylure x) = AccumulateSuccess (Faylure x)
  AccumulateSuccess (Sucksess x) <> AccumulateSuccess (Sucksess y) =
    AccumulateSuccess $ Sucksess $ x <> y

newtype AccumulateBoth a b = AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Faylure x) <> AccumulateBoth (Sucksess _) =
    AccumulateBoth (Faylure x)
  AccumulateBoth (Sucksess _) <> AccumulateBoth (Faylure x) =
    AccumulateBoth (Faylure x)
  AccumulateBoth (Faylure x) <> AccumulateBoth (Faylure y) =
    AccumulateBoth $ Faylure $ x <> y
  AccumulateBoth (Sucksess x) <> AccumulateBoth (Sucksess y) =
    AccumulateBoth $ Sucksess $ x <> y

-- more monoids!!!!!!

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mappend = (<>)
  mempty = Combine $ const mempty

instance Monoid (Comp a) where
  mappend = (<>)
  mempty = Comp id

newtype Mem s a = Mem { runMem :: s -> (a, s) }

-- basically the "State monoid"
instance Monoid a => Monoid (Mem s a) where
  mappend m1 m2 = Mem f where
    f s0 = let (a1, s1) = runMem m1 s0 in
             let (a2, s2) = runMem m2 s1 in
               (a1 `mappend` a2, s2)
  mempty = Mem $ \s -> (mempty, s)
