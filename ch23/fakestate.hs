newtype FakeState s a = FakeState { runFakeState :: s -> (a, s) }

instance Functor (FakeState s) where
  fmap f m = FakeState $ \s -> let (a, s') = runFakeState m s in (f a, s')

instance Applicative (FakeState s) where
  pure x = FakeState $ \s -> (x, s)
  a <*> m = FakeState $ \s -> let (f, s') = runFakeState a s
                                  (x, s'') = runFakeState m s' in
                                  (f x, s'')

instance Monad (FakeState s) where
  m >>= k = FakeState $ \s -> let (x, s') = runFakeState m s in
                                  runFakeState (k x) s'

fizzBuzz1 :: (Integral a, Show a) => a -> String 
fizzBuzz1 n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzBuzz :: (Integral a, Show a) => a -> [String]
fizzBuzz = fmap fizzBuzz1 . enumFromTo 1
