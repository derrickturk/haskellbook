module Ch12 where

import Data.List (intersperse)

-- sorry, I don't see the point of their helper function
replaceThe :: String -> String
replaceThe = concat . intersperse " " . map f . words where
  f w
    | w == "the" = "a"
    | otherwise = w

data Nat = Zero
         | Succ Nat
         deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger = natToInteger' 0 where
  natToInteger' acc Zero = acc
  natToInteger' acc (Succ n) = natToInteger' (acc + 1) n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ integerToNat' n where
      integerToNat' 0 = Zero
      integerToNat' n = Succ $ integerToNat' (n - 1)

-- "maybe library"

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d _ Nothing = d
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x:catMaybes xs

-- aka sequence
flipMaybes :: [Maybe a] -> Maybe [a]
flipMaybes [] = Just []
flipMaybes (Nothing:_) = Nothing
flipMaybes (Just x:xs) = (x :) <$> flipMaybes xs

{-- long form:
flipMaybes (Just x:xs) = case flipMaybes xs of
  Just rest -> Just (x:rest)
  Nothing -> Nothing
--}

-- "either library"
lefts' :: [Either a b] -> [a]
lefts' = foldr appendIf [] where
  appendIf (Left x) lefts = x:lefts
  appendIf (Right _) lefts = lefts

rights' :: [Either a b] -> [b]
rights' = foldr appendIf [] where
  appendIf (Right x) rights = x:rights
  appendIf (Left _) rights = rights

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = either' (const Nothing) (Just . f)

-- unfolds

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = maybe [] (\(y, next) -> y:myUnfoldr f next) (f x)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))
