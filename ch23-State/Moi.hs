{-# LANGUAGE InstanceSigs #-}
main :: IO ()
main = do
  print $ runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0
  print $ runMoi (pure (*2) <*> pure 3) 4
  let m = return 3
  print $ runMoi (m >>= (\x -> Moi $ \s -> (x + s, s))) 5
  print $ runMoi get "curryIsAmaze"
  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"
  print $ eval get "bunnicula"
  print $ eval get "stake a bunny"
  print $ runMoi (modify (+1)) 0
  print $ runMoi (modify (+1) >> modify (+2)) 0

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

-- FUNCTIONS

get :: Moi s s
get =
  Moi $ \s -> (s, s)

put :: s -> Moi s ()
put s =
  Moi $ const ((), s)

exec :: Moi s a -> s -> s
exec (Moi f) =
  snd . f

eval :: Moi s a -> s -> a
eval (Moi f) =
  fst . f

modify :: (s -> s) -> Moi s ()
modify f =
  Moi $ \s -> ((), f s)
