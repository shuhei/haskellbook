module Main where

import Traversable

main = do
  -- `(: [])` is equivalent to `\x -> x : []` and `\x -> [x]`
  print $ traverse (: []) (Identity 3)
  print $ traverse (\x -> if x == 3 then Just x else Nothing) $ Cons 3 (Cons 4 Nil)
