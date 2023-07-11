module Ch9 where

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

enumFromToBool :: Bool -> Bool -> [Bool]
enumFromToBool False False = [False]
enumFromToBool False True = [False, True]
enumFromToBool True False = []
enumFromToBool True True = [True]

enumFromToInt :: Int -> Int -> [Int]
enumFromToInt n m
  | n <= m = n:enumFromToInt (n + 1) m
  | otherwise = []

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split d xs = (:)
  (takeWhile (/= d) xs)
  (split d $ dropWhile (== d) . dropWhile (/= d) $ xs)

myWords = split ' '
myLines = split '\n'

dropArticles :: String -> [String]
dropArticles = filter (not . (`elem` ["the", "a", "an"])) . words

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

zip'' = zipWith' (,)

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (False:xs) = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = myOr . map p

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "empty list"
myMaximumBy f (x:[]) = x
myMaximumBy f (x:xs) = let y = myMaximumBy f xs in case f x y of
  GT -> x
  _ -> y

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare
