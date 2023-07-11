{-# LANGUAGE FlexibleInstances #-} -- just for one spot

module Ch16 where

a = (+ 1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (* 2) <$> (\x -> x - 2)
c' = (* 2) . (\x -> x - 2)

d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])
d' = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . ("123" ++) . show <$> ioi
    in (* 3) <$> changed

data Quant a b = Finance | Desk a | Bloor b
-- this is actually somewhat subtle:
--   the second two patterns can't be replaced by
--   fmap _ x = x
--   because that wouldn't type check:
--   we have fmap :: (b -> c) -> Quant a b -> Quant a c
--   so we have to deconstruct the Quant a b (even if it doesn't
--     contain a b) and construct a Quant a c
instance Functor (Quant a) where
  fmap f (Bloor x) = Bloor (f x)
  fmap _ (Desk x) = Desk x
  fmap _ Finance = Finance

newtype K a b = K a -- isn't this isomorphic to Constant?
instance Functor (K a) where -- yeah, it is
  fmap _ (K x) = K x

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- we need FlexibleInstances to let us do this (otherwise we'd only
--   be able to make an instance for Flip f a b)
instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K $ f x)

data EvilConst a b = EvilConst b

instance Functor (EvilConst a) where
  fmap f (EvilConst x) = EvilConst $ f x

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut x) = LiftItOut (f <$> x)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (f <$> x) (f <$> y)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (f <$> y)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (f <$> z)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a)
                            (GoatLord a)
                            (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read (f <$> g)
