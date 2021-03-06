class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g =
    first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b
  = Deux a b
  deriving Show

instance Bifunctor Deux where
  bimap f g (Deux a b) =
    Deux (f a) (g b)

data Const a b
  = Const a
  deriving Show

instance Bifunctor Const where
  bimap f g (Const a) =
    Const (f a)

data Drei a b c
  = Drei a b c
  deriving Show

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) =
    Drei a (f b) (g c)

data SuperDrei a b c
  = SuperDrei a b
  deriving Show

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei a b) =
    SuperDrei a (f b)

data SemiDrei a b c
  = SemiDrei a
  deriving Show

instance Bifunctor (SemiDrei a) where
  bimap f g (SemiDrei a) =
    SemiDrei a

data Quadriceps a b c d
  = Quadriceps a b c d
  deriving Show

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadriceps a b c d) =
    Quadriceps a b (f c) (g d)

instance Bifunctor Either where
  bimap f g (Left a) = Left (f a)
  bimap f g (Right b) = Right (g b)
