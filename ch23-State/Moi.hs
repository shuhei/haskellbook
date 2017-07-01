{-# LANGUAGE InstanceSigs #-}
main :: IO ()
main = do
  print $ runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
  print $ runMoi (pure (*2) <*> pure 3) 4
  let m = return 3
  print $ runMoi (m >>= (\x -> Moi $ \s -> (x + s, s))) 5

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \s ->
      let (a, s1) = g s
      in (f a, s1)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure x =
    Moi $ \s -> (x, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (ab, s1) = f s
          (a, s2) = g s1
      in (ab a, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a
        -> (a -> Moi s b)
        -> Moi s b
  (Moi f) >>= g =
    Moi $ \s ->
      let (a, s1) = f s
          (Moi sb) = g a
      in sb s1
