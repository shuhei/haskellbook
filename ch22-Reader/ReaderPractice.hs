import Control.Applicative
import Data.Maybe

main = do
  mapM_ print [xs, ys, zs, z' 1]
  mapM_ print [x1, x2, x3 2]
  print $ summed (4, 9)
  mapM_ (print . bolt) [1, 3, 4, 7, 8, 9]
  print $ sequenceA [(>3), (<8), even] 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  print $ and $ sequA 2

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs = lookup 3 $ zip x y
ys = lookup 6 $ zip y z
zs = lookup 4 $ zip x z
z' n = lookup n $ zip x z

x1 = (,) <$> xs <*> ys
x2 = (,) <$> ys <*> zs
x3 n = (,) <$> z' n <*> z' n

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> liftA2 (,) xs ys
