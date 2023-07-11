module Ch18 where

import Control.Monad (join, (>=>), (<=<), (=<<), liftM2)

bind :: Monad m => (a -> m b) -> m a -> m b
bind = (join .) . fmap

data Sum a b = First a 
             | Second b
             deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second x) = Second $ f x

{--
instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  _ <*> First x = First x
  Second f <*> Second x = Second $ f x
--}

-- a nice idea from the official version:
instance Applicative (Sum a) where
  pure = Second
  First x <*> _ = First x
  Second f <*> x = fmap f x

instance Monad (Sum a) where
  -- we get this automatically:
  -- return = pure 
  First x >>= _ = First x
  Second x >>= f = f x

mcomp :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
mcomp g f x = f x >>= g

-- this is join
j :: Monad m => m (m a) -> m a
j x = x >>= id

-- this is liftM / liftA / fmap
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- this is liftM2 / liftA2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- motherfuck this fucking motherfucker
-- this is "forM" == flip mapM
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

-- this is "sequence"
flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id

-- now I can see it's just liftM2'd equivalent of map
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = liftM2 (:) (f x) (mapM' f xs)

-- e.g.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (:) (f x) (map' f xs)

-- you can also do it with a fold, e.g.
mapM'' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM'' f = foldr (\x ms -> liftM2 (:) (f x) ms) (return [])

-- or with do-notation
mapM''' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM''' _ [] = return []
mapM''' f (x:xs) = do
  y <- f x -- :: m b
  ys <- mapM''' f xs -- :: m [b]
  return $ y:ys

-- also, doesn't require Monad
-- also, I believe this is "traverse"
mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA _ [] = pure []
mapA f (x:xs) = (:) <$> f x <*> mapA f xs
