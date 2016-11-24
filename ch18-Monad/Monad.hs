module Monad where

import Prelude hiding (Monad, (>>=), (>>), return, Left, Right)

class Applicative m => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= const y

-- Nope a

data Nope a = NopeDotJpg

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg

  _ <*> _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

-- PhhhbbtttEither b a

data PhhhbbtttEither b a
  = Left a
  | Right b

instance Functor (PhhhbbtttEither b) where
  fmap f (Left x) = Left (f x)
  fmap _ (Right x) = Right x

instance Applicative (PhhhbbtttEither b) where
  pure = Left

  (Left f) <*> x = fmap f x
  (Right x) <*> _ = Right x

instance Monad (PhhhbbtttEither b) where
  (Left x) >>= f = f x
  (Right x) >>= _ = Right x

-- Identity a

data Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> x = fmap f x

instance Monad Identity where
  (Identity x) >>= f = f x

-- List a

data List a
  = Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil

  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

instance Monoid (List a) where
  mempty = Nil

  mappend Nil ys = ys
  mappend xs Nil = xs
  mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Monad List where
  Nil >>= _ = Nil
  Cons x xs >>= f = f x `mappend` (xs >>= f)
