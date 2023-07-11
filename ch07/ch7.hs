{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch7 where

nums :: (Num a, Ord a, Num b) => a -> b
nums x = case compare x 0 of
    EQ -> 0
    LT -> -1
    GT -> 1

abs' :: (Num a, Ord a) => a -> a
abs' x
    | x < 0 = (-x)
    | otherwise = x

-- why would you write it this way?
tensDigit = snd . flip divMod 10 . flip div 10

placeDigit place = flip mod place . flip div place
tensDigit' = placeDigit 10
hundredsDigit = placeDigit 100

pickBool :: a -> a -> Bool -> a
pickBool x y b = case b of
    True -> x
    False -> y

pickBool' :: a -> a -> Bool -> a
pickBool' x y b
    | b == True = x
    | b == False = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

rt4 = roundTrip 4 :: Int
