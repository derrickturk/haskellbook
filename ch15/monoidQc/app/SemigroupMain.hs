module Main where

import Lib
import Test.QuickCheck
import Test.QuickCheck.Function
import Data.Semigroup

type AssocType a = a -> a -> a -> Bool

semigroupAssoc :: (Eq a, Semigroup a) => AssocType a
semigroupAssoc a b c = (a <> b) <> c == a <> (b <> c)

semigroupAssocComp :: (Eq a) => a -> AssocType (Fun a a)
semigroupAssocComp x (Fun _ f) (Fun _ g) (Fun _ h) =
  left x == right x where
    left = unComp $ ((Comp f <> Comp g) <> Comp h)
    right = unComp $ (Comp f <> (Comp g <> Comp h))

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

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

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
    Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = oneof [Faylure <$> arbitrary, Sucksess <$> arbitrary]

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateSuccess a b) where
  arbitrary = AccumulateSuccess <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = AccumulateBoth <$> arbitrary

main = do
  quickCheck (semigroupAssoc :: AssocType (Trivial))
  quickCheck (semigroupAssoc :: AssocType (Identity (Sum Int)))
  quickCheck (semigroupAssoc :: AssocType (Two String (Sum Int)))
  quickCheck
    (semigroupAssoc :: AssocType (Three String [Int] (Identity String)))
  quickCheck (semigroupAssoc :: AssocType (Four [Int] [Int] [Int] String))
  quickCheck (semigroupAssoc :: AssocType BoolConj)
  quickCheck (semigroupAssoc :: AssocType BoolDisj)
  quickCheck (semigroupAssoc :: AssocType (Or [Int] (Sum Double)))
  quickCheck (semigroupAssoc :: AssocType (Or [Int] (Sum Double)))
  quickCheck (semigroupAssoc :: AssocType (Validation String Int))
  quickCheck (semigroupAssoc :: AssocType (AccumulateSuccess Int String))
  quickCheck (semigroupAssoc :: AssocType (AccumulateBoth String [Int]))
  quickCheck (semigroupAssocComp :: Int -> AssocType (Fun Int Int))
