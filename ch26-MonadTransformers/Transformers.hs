{-# LANGUAGE InstanceSigs #-}

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
