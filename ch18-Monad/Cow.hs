module Cow where

import Control.Monad (liftM3)

data Cow
  = Cow { name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty x = Just x

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0    = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
  liftM3 Cow (noEmpty name') (noNegative age') (noNegative weight')
  >>= weightCheck
