module BiFunctor where

class BiFunctor f where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
  bimap f g = first f . second g

  first :: (a -> b) -> f a c -> f b c
  first f = bimap f id

  second :: (c -> d) -> f a c -> f a d
  second g = bimap id g

data Deux a b = Deux a b
data Const a b = Const a
data Drei a b c = Drei a b c
data SuperDrei a b c = SuperDrei a b
data SemiDrei a b c = SemiDrei a
data Quadriceps a b c d = Quadzzz a b c d
data Either' a b = Left' a
                 | Right' b

instance BiFunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

instance BiFunctor Const where
  bimap f _ (Const x) = Const $ f x

instance BiFunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

instance BiFunctor (SuperDrei a) where
  bimap f _ (SuperDrei x y) = SuperDrei x (f y)

instance BiFunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

instance BiFunctor (Quadriceps a b) where
  bimap f g (Quadzzz w x y z) = Quadzzz w x (f y) (g z)

instance BiFunctor Either' where
  bimap f _ (Left' x) = Left' $ f x
  bimap _ g (Right' y) = Right' $ g y
