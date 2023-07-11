module Main where

import Criterion.Main

(!?) :: [a] -> Int -> Maybe a
infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)

myList :: [Int]
myList = [1..9999]

main :: IO ()
main = defaultMain
  [ bench "myList !! 9998" $ whnf (myList !!) 9998
  , bench "myList !? 9998" $ whnf (myList !?) 9998
  ]
