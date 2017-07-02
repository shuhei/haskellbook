{-# LANGUAGE InstanceSigs #-}
module Traversable where

newtype Identity a
  = Identity a
  deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity a) =
    Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) =
     f a

instance Traversable Identity where
  traverse f (Identity a) =
    fmap Identity (f a)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) =
    f x `mappend` foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) =
    Cons <$> f x <*> traverse f xs

data S n a
  = S (n a) a
  deriving (Show, Eq)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) $ f a

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na `mappend` f a

instance Traversable n => Traversable (S n) where
  traverse :: Applicative f
    => (a -> f b)
    -> S n a
    -> f (S n b)
  traverse f (S na a) =
    S <$> traverse f na <*> f a

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) =
    Node <$> traverse f l <*> f x <*> traverse f r
