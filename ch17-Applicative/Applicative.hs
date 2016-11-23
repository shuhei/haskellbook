{-# LANGUAGE InstanceSigs #-}

-- Chapter 17. Applicative: Exercise
module App where

import Prelude hiding (Applicative, pure, (<*>))

class Functor f => Applicative f where
  pure :: a -> f a

  (<*>) :: f (a -> b) -> f a -> f b

-- []

instance Applicative [] where
  pure :: a -> [a]
  pure x = [x]

  (<*>) :: [a -> b] -> [a] -> [b]
  fs <*> xs = zipWith ($) fs xs

-- IO

instance Applicative IO where
  pure :: a -> IO a
  pure = return

  (<*>) :: IO (a -> b) -> IO a -> IO b
  fio <*> x = do
    f <- fio
    fmap f x
  -- Or
  -- fio <*> x = fio >>= (`fmap` x)

-- (,) a

instance Monoid a => Applicative ((,) a) where
  pure :: b -> (a, b)
  pure x = (mempty, x)

  (<*>) :: (a, b -> c) -> (a, b) -> (a, c)
  (a1, f) <*> (a2, x) = (mappend a1 a2, f x)

-- (->) a

instance Applicative ((->) a) where
  pure :: b -> a -> b
  pure x _ = x

  (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
  f <*> g = \x -> f x (g x)

-- Pair a

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure :: a -> Pair a
  pure x = Pair x x

  (<*>) :: Pair (a -> b) -> Pair a -> Pair b
  (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

-- Two a b

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure :: b -> Two a b
  pure = Two mempty

  (<*>) :: Two a (b -> c) -> Two a b -> Two a c
  Two a1 f <*> Two a2 x = Two (mappend a1 a2) (f x)

-- Three a b c

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure :: c -> Three a b c
  pure = Three mempty mempty

  (<*>) :: Three a b (c -> d) -> Three a b c -> Three a b d
  Three a1 b1 f <*> Three a2 b2 x = Three (mappend a1 a2) (mappend b1 b2) (f x)

-- Three' a b

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure :: b -> Three' a b
  pure x = Three' mempty x x

  (<*>) :: Three' a (b -> c) -> Three' a b -> Three' a c
  Three' a1 f g <*> Three' a2 x y = Three' (mappend a1 a2) (f x) (g y)

-- Four a b c d

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty

  Four a1 b1 c1 f <*> Four a2 b2 c2 x =
    Four (mappend a1 a2) (mappend b1 b2) (mappend c1 c2) (f x)
