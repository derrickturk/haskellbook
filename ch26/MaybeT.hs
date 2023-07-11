module MaybeT where

import Control.Applicative (liftA2)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  -- pull out the m (Maybe a), apply f lifted over both levels, wrap in MaybeT
  fmap f = MaybeT . (fmap . fmap) f . runMaybeT

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  -- just like Compose: once you know the trick, whatever
  (MaybeT f) <*> (MaybeT x) = MaybeT $ (<*>) <$> f <*> x

instance Monad m => Monad (MaybeT m) where
  -- m :: m (Maybe a)
  -- k :: a -> MaybeT m b
  -- runMaybeT . k :: a -> m (Maybe b)
  -- need f :: (Maybe a) -> m (Maybe b)
  -- there might be a cleaner way, or not (not)
  (MaybeT m) >>= k = MaybeT $ m >>= f where
    f Nothing = pure Nothing
    f (Just x) = runMaybeT $ k x
