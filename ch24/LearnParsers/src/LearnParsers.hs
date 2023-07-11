module LearnParsers where

import Data.Ratio ((%))
import Control.Applicative
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

oneTwo :: Parser Char
oneTwo = do
  char '1'
  char '2'

oneTwo' :: Parser a
oneTwo' = oneTwo >> stop

parseFraction :: Parser Rational
parseFraction = do
  num <- decimal
  token $ char '/'
  den <- decimal
  if den == 0
    then fail "zero denominator" -- how to back out the last parse?
    else return (num % den)

parseDecimal :: Parser Rational
parseDecimal = do
  whole <- decimal
  token $ char '.'
  fractional <- decimal
  let fractionalDigits = length $ show fractional -- lol
  return $ whole % 1 + fractional % (10 ^ fractionalDigits - 1)

parseRational :: Parser Rational
parseRational = try parseFraction <|> parseDecimal
