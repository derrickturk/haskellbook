palindrome :: Eq a => [a] -> Bool
palindrome x = x == reverse x

abs' :: Integer -> Integer
abs' x = if x < 0 then -x else x
