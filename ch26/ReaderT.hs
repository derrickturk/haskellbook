module ReaderT where

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f = ReaderT . (fmap . fmap) f . runReaderT

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  -- just like Compose: once you know the trick, whatever
  (ReaderT f) <*> (ReaderT x) = ReaderT $ (<*>) <$> f <*> x

instance Monad m => Monad (ReaderT r m) where
  m >>= k = ReaderT $ \r -> do
    -- here we are do'ing in the m monad
    x <- runReaderT m r
    runReaderT (k x) r
