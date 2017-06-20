{-# LANGUAGE InstanceSigs #-}
module Compose (Identity(..), Compose(..)) where

-- Identity

newtype Identity a =
  Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

-- Compose

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) =>
         Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure x = Compose $ (pure . pure) x

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ fmap (<*>) f <*> a

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) =
    (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse :: Applicative h
           => (a -> h b)
           -> Compose f g a
           -> h (Compose f g b)
  traverse f (Compose fga) =
    Compose <$> (traverse . traverse) f fga

-- One

newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

-- Three

newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) =>
         Functor (Three f g h) where
  fmap :: (a -> b) -> Three f g h a -> Three f g h b
  fmap f (Three fgha) =
    Three $ (fmap . fmap . fmap) f fgha
