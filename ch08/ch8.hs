fibs :: (Num a) => [a]
fibs = 1:1:zipWith (+) fibs (tail fibs)

fibth :: (Num a) => Int -> a
fibth = (fibs !!)

fibrec :: (Integral a) => a -> a
fibrec 0 = 1
fibrec 1 = 1
fibrec n = fibrec (n - 1) + fibrec (n - 2)

-- n.b. offside fuckery with where + guards
divrec :: Integer -> Integer -> Maybe (Integer, Integer)
divrec _ 0 = Nothing
divrec n d = Just $ divrec' n d where
  divrec' n d
    | n >= d = let (q, r) = divrec' (n - d) d in (q + 1, r)
    | otherwise = (0, n)

divrecAcc :: Integral a => a -> a -> Maybe (a, a)
divrecAcc _ 0 = Nothing
divrecAcc n d = Just $ go n d 0 where
  go n d acc
    | n < d = (acc, n)
    | otherwise = go (n - d) d (acc + 1)

mulrec :: Integral a => a -> a -> a
mulrec _ 0 = 0
mulrec n m = n + mulrec n (m - 1)

mc91 :: Integral a => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 $ n + 11
