module StateT where

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT x) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (x s)

-- you need Monad as a constraint to ensure that you can order things
-- properly...
instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  (StateT mab) <*> (StateT ma) = StateT $ \s -> do
    (f, s') <- mab s
    (x, s'') <- ma s'
    return $ (f x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= k = StateT $ \s -> do
    (x, s') <- runStateT m s
    runStateT (k x) s'
