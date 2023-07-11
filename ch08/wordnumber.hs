module WordNumber where

import Data.List (intersperse)

wordDigit :: Integral a => a -> String
wordDigit 0 = "zero"
wordDigit 1 = "one"
wordDigit 2 = "two"
wordDigit 3 = "three"
wordDigit 4 = "four"
wordDigit 5 = "five"
wordDigit 6 = "six"
wordDigit 7 = "seven"
wordDigit 8 = "eight"
wordDigit 9 = "nine"
wordDigit _ = error "not a digit"

digits :: Integral a => a -> [a]
digits 0 = [0] -- fuck this case in particular
digits n = reverse $ digitsRev n
  where digitsRev 0 = []
        digitsRev n = n `mod` 10 : digitsRev (n `div` 10)

wordNumber :: Integral a => a -> String
wordNumber = concat . intersperse "-" . map wordDigit . digits
