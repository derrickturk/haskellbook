module Main where

import qualified Data.Map as M
import Morse
import Test.QuickCheck 

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.keys morseToLetter

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_roundTrip :: Property
prop_roundTrip = forAll charGen
  (\c -> (charToMorse c >>= morseToChar) == Just c)

main :: IO ()
main = quickCheck prop_roundTrip
