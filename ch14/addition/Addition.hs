module Addition where

import Test.Hspec
import Test.QuickCheck

sayHello :: IO ()
sayHello = putStrLn "hello"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy n d = go n d 0 where
  go n d q
    | n < d = (q, n)
    | otherwise = go (n - d) d (q + 1)

mul :: (Eq a, Num a) => a -> a -> a
mul n m = go n m 0 where
  go n 0 a = a
  go n m a = go n (m - 1) (a + n)

main :: IO ()
main = hspec $ describe "Addition" $
                 (it "15 / 3 is 5" $
                   15 `dividedBy` 3 `shouldBe` (5, 0)) >>
                 (it "22 / 5 = 4 rem 2" $
                   22 `dividedBy` 5 `shouldBe` (4, 2)) >>
                 (it "7 * 3 = 21" $
                   7 `mul` 3 `shouldBe` 21) >>
                 (it "0 * 6 = 0" $
                   0 `mul` 6 `shouldBe` 0) >>
                 (it "x + 1 is always greater than x" $
                   property $ \x -> x + 1 > (x :: Int))

propAddIncreases :: Int -> Bool
propAddIncreases x = x + 1 > x

qcOnly :: IO ()
qcOnly = quickCheck propAddIncreases
