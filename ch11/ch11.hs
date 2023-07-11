module Ch11 where

import Data.Char (toUpper, isLetter)

-- as-pattern exercises

-- n.b. just like the problem asks, the elements don't have to
--   be contiguous in the second arg
isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf lis@(x:xs) (y:ys) =
  x == y && isSubseqOf xs ys || isSubseqOf lis ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map f . words where
  f "" = ("", "")
  f word@(first:rest) = (word, (toUpper first):rest)

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (f:r) = (toUpper f):r

-- gross
capitalizeParagraph :: String -> String
capitalizeParagraph "" = ""
capitalizeParagraph p =
  (capitalizeWord (takeWhile (/= '.') p)) ++
  (takeWhile (not . isLetter) . dropWhile (/= '.') $ p) ++
  (capitalizeParagraph (dropWhile (not . isLetter) . dropWhile (/= '.') $ p))
