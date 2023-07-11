thirdLetter :: String -> Char
thirdLetter s = s !! 2

betterThird :: String -> Maybe Char
betterThird = nth 2
  where nth :: Int -> [a] -> Maybe a
        nth _ [] = Nothing
        nth 0 (x:xs) = Just x
        nth n (x:xs) = nth (n - 1) xs
