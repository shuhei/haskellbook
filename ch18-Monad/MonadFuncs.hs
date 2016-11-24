module MonadFuncs where

-- join

j :: Monad m => m (m a) -> m a
j x = x >>= id

-- liftM

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- liftM2

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f x y = f <$> x <*> y

-- <**>

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- forM

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) f = l2 (:) (f x) (meh xs f)

-- sequence

flipType :: Monad m => [m a] -> m [a]
flipType xs = meh xs id
