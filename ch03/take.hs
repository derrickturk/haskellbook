take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:(take (n - 1) xs)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 x = x
drop' n (x:xs) = drop' (n - 1) xs
