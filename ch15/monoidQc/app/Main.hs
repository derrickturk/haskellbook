module Main where

import Lib
import Test.QuickCheck
import Data.Monoid

rightIdent :: (Eq a, Monoid a) => a -> Bool
rightIdent m = m <> mempty == m

leftIdent :: (Eq a, Monoid a) => a -> Bool
leftIdent m = mempty <> m == m

assoc :: (Eq a, Monoid a) => AssocType a
assoc x y z = (x <> y) <> z == x <> (y <> z)

type AssocType a = a -> a -> a -> Bool

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = arbitrary >>= \x -> oneof [ return Nada, return $ Only x ]

instance Arbitrary a => Arbitrary (Firstish a) where
  arbitrary = Firstish <$> arbitrary

main :: IO ()
main = do
  quickCheck (rightIdent :: (Optional [Int]) -> Bool)
  quickCheck (leftIdent :: (Optional [Int]) -> Bool)
  quickCheck (assoc :: AssocType (Optional [Int]))
  quickCheck (rightIdent :: (Firstish [Int]) -> Bool)
  quickCheck (leftIdent :: (Firstish [Int]) -> Bool)
  quickCheck (assoc :: AssocType (Firstish [Int]))
