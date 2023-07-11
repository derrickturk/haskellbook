module Ch10 where

import Data.List (foldl', foldl1')

stops = "pbtdkg"
vowels = "aeiou"

maybeWords :: String -> String -> [(Char, Char, Char)]
maybeWords stops vowels = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]


maybeWordsBeginP s v = filter ((== 'p') . fst3) $ maybeWords s v
  where fst3 (a, _, _) = a

avgWordLength :: String -> Int
avgWordLength xs = div (sum $ map length $ words xs)
                       (length $ words xs)

avgWordLength' :: Fractional a => String -> a
avgWordLength' xs = (sum $ map (fromIntegral . length) $ words xs) /
                    (fromIntegral $ length $ words xs)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- I don't see a clean way to write this completely point-free
myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = foldr (\x b -> p x || b) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem e xs = foldr (\x b -> x == e || b) False xs


myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

-- first attempt
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- hell yeah
myMap' :: (a -> b) -> [a] -> [b]
myMap' = (flip foldr []) . ((:) .)

-- this can probably be point-free'd completely somehow
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x:xs else xs) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f -- monad laws bitches

-- or if we insist
squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr (\x xs -> f x ++ xs) []

-- or if we squint
squishMap'' :: (a -> [b]) -> [a] -> [b]
squishMap'' = (flip foldr []) . ((++) .)

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- I aint inventing no damn element
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f = foldl1'
  (\m x -> case f x m of
    GT -> x
    _ -> m)
