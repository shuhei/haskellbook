module Main where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import System.Process (system)
import System.Environment (getArgs)
import System.IO (stdout, hSetBuffering, BufferMode (NoBuffering))

type Scores = (Int, Int)

data Guess =
  Guess { guessNumber :: Int
        , guessTotal :: Int
        }
  deriving (Show)

data Config =
  Config { vsHuman :: Bool
         , playerName1 :: String
         , playerName2 :: String
         }

-- Get input until a parsable value is provided and it matches the given predicate
readPersistently :: Read a => (a -> Bool) -> String -> IO a
readPersistently p msg = go
  where
    tryAgain = putStrLn msg >> go
    go = do
      rs <- reads <$> getLine
      case rs of
        [(x, "")] -> if p x then return x else tryAgain
        _ -> tryAgain

isInRange :: Int -> Int -> Int -> Bool
isInRange from to n =
  from <= n && n <= to

readGuess :: String -> IO Guess
readGuess name = do
  putStr $ "(" ++ name ++ ") number: "
  n <- readPersistently (isInRange 0 5) "Input an integer from 0 to 5"
  putStr $ "(" ++ name ++ ") total: "
  total <- readPersistently (isInRange 0 10) "Input an integer from 0 to 10"
  system "clear"
  return $ Guess n total

randomGuess :: IO Guess
randomGuess = do
  n <- randomRIO (0, 5)
  total <- (+ n) <$> randomRIO (0, 5)
  return $ Guess n total

hasWon :: Int -> Guess -> Bool
hasWon total (Guess _ guessed) =
  total == guessed

showResult :: Int -> Guess -> String
showResult total guess =
  (if hasWon total guess then "Won!" else "Lost!") ++ " " ++ show guess

-- ReaderT might be too much here, but this is an exercise for Monad Transformers!
step :: ReaderT Config (StateT Scores IO) ()
step = do
  Config withHuman name1 name2 <- ask
  guess1 <- liftIO $ readGuess name1
  guess2 <- liftIO $ if withHuman then readGuess name2 else randomGuess

  let total = guessNumber guess1 + guessNumber guess2
  (score1, score2) <- lift get
  let newScore1 = score1 + if hasWon total guess1 then 1 else 0
      newScore2 = score2 + if hasWon total guess2 then 1 else 0
  lift $ put (newScore1, newScore2)

  liftIO $ do
    putStrLn $ name1 ++ ": " ++ showResult total guess1
    putStrLn $ name2 ++ ": " ++ showResult total guess2
    putStrLn $ "Actual total: " ++ show total
    putStrLn $ "New scores: (" ++ name1 ++ ") " ++ show newScore1 ++ " vs (" ++ name2 ++ ") " ++ show newScore2
    putStrLn ""

-- Keep running StateT until its state matches the given predicate
loopStateT :: (Monad m) => (s -> Bool) -> s -> StateT s m a -> m s
loopStateT isDone s st = do
  (_, newState) <- runStateT st s
  if isDone newState then
    return newState
  else
    loopStateT isDone newState st

makeConfig :: [String] -> Config
makeConfig args =
  if "-h" `elem` args then
    Config True "Human 1" "Human 2"
  else
    Config False "Human" "Computer"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  config@(Config _ name1 name2) <- makeConfig <$> getArgs
  (s1, s2) <- loopStateT shouldExit (0, 0) $ runReaderT step config
  when (isWinner s1) $ putStrLn $ "Conguratulations, " ++ name1 ++ "!"
  when (isWinner s2) $ putStrLn $ "Conguratulations, " ++ name2 ++ "!"
  return ()
  where
    isWinner = (>= 3)
    shouldExit (s1, s2) = isWinner s1 || isWinner s2
