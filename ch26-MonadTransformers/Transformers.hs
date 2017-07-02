{-# LANGUAGE InstanceSigs #-}

import qualified Control.Arrow as A

-- MaybeT

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) =
    MaybeT $ (fmap . fmap) f mma

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure x = MaybeT $ Just <$> pure x

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mmab) <*> (MaybeT mma) =
    MaybeT $ (<*>) <$> mmab <*> mma

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f =
    MaybeT $ do
      ma <- mma
      case ma of
        Nothing -> return Nothing
        Just a -> runMaybeT $ f a

-- EitherT

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) =
    EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure x = EitherT $ Right <$> pure x

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT meab) <*> (EitherT mea) =
    EitherT $ (<*>) <$> meab <*> mea

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea) >>= f =
    EitherT $ do
      ea <- mea
      case ea of
        Left e -> return $ Left e
        Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) =
  EitherT $ swapEither <$> mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = do
  ea <- mea
  case ea of
    Left e -> f e
    Right a -> g a

-- ReaderT

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT $ const $ pure x

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT rmab) <*> (ReaderT rma) =
    ReaderT $ \r -> rmab r <*> rma r

instance (Monad m) => Monad (ReaderT r m) where
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

-- StateT

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) =
    StateT $ \s -> A.first f <$> sma s

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) =
    StateT $ \s -> do
      (ab, s1) <- smab s
      (a, s2) <- sma s1
      return (ab a, s2)

instance (Monad m) => Monad (StateT s m) where
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s1) <- sma s
      runStateT (f a) s1
