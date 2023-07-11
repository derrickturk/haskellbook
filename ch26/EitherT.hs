-- jsut for fucking show!!! RFEEEEEEEEEEEEEEEEEEE
{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}

module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

deriving instance (Show e, Show (m (Either e a)), Show a) => Show (EitherT e m a)

instance Functor m => Functor (EitherT e m) where
  fmap f = EitherT . (fmap . fmap) f . runEitherT

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  -- just like Compose: once you know the trick, whatever
  (EitherT f) <*> (EitherT x) = EitherT $ (<*>) <$> f <*> x

instance Monad m => Monad (EitherT e m) where
  -- m :: m (EitherT e m a)
  -- k :: a -> (EitherT e m b)
  -- runEitherT . k :: a -> m (Either e b)
  -- need f :: (Either e a) -> m (Either e b)
  (EitherT m) >>= k = EitherT $ m >>= f where
    f (Left e) = pure $ Left e
    f (Right x) = runEitherT $ k x

swapEither :: Either e a -> Either a e
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . fmap swapEither . runEitherT

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT l r = (>>= (either l r)) . runEitherT
