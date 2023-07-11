module CoreEx where

data Test = A Test2 | B Test2
data Test2 = C | D

match1 :: Test -> Bool
match1 (A _) = True
match1 (B _) = False

match2 :: Test -> Bool
match2 (A C) = True
match2 (B C) = True
match2 (A D) = False
match2 (B D) = False
