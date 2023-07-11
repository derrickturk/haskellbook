module Main where

import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . (fmap . fmap) f . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT x) = EitherT $ (<*>) <$> f <*> x

instance Monad m => Monad (EitherT e m) where
  (EitherT m) >>= k = EitherT $ m >>= f where
    f (Left e) = pure $ Left e
    f (Right x) = runEitherT $ k x

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

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

instance MonadTrans (StateT s) where
  -- lift :: m a -> StateT s m a
  lift m = StateT $ \s -> m >>= \x -> return (x, s)

instance MonadIO m => MonadIO (StateT s m) where
  -- liftIO :: IO a -> StateT s m a
  liftIO = lift . liftIO

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT l r = (>>= (either l r)) . runEitherT

main :: IO ()
main = do
  putStrLn "hello world"
