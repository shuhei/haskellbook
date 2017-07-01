import Control.Monad
import Control.Monad.Trans.State
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "from: "
  from <- read <$> getLine
  putStr "to: "
  to <- read <$> getLine
  mapM_ putStrLn $ fizzBuzzFromTo from to

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  5 == 0 = "Fizz"
  | n `mod`  3 == 0 = "Buzz"
  | otherwise       = show n

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
  let reversedList = enumFromThenTo to (to - 1) from
  in execState (mapM_ addResult reversedList) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put $ result : xs
