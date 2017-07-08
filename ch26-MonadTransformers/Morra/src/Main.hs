{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)

type Scores = (Int, Int)

data Guess =
  Guess { guessNumber :: Int, guessTotal :: Int }
  deriving (Show)

readGuess :: IO Guess
readGuess = do
  n <- read <$> getLine
  total <- read <$> getLine
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

step :: StateT Scores IO ()
step = do
  playerGuess <- liftIO readGuess
  aiGuess <- liftIO randomGuess

  let total = guessNumber playerGuess + guessNumber aiGuess
  (playerScore, aiScore) <- get
  let newPlayerScore = playerScore + if hasWon total playerGuess then 1 else 0
      newAiScore = aiScore + if hasWon total aiGuess then 1 else 0
  put (newPlayerScore, newAiScore)

  liftIO $ do
    putStrLn $ "Player: " ++ showResult total playerGuess
    putStrLn $ "AI: " ++ showResult total aiGuess
    putStrLn $ "Actual total: " ++ show total
    putStrLn $ "New scores: Player " ++ show newPlayerScore ++ " vs AI " ++ show newAiScore

loopStateT :: (Monad m) => (s -> Bool) -> s -> StateT s m a -> m s
loopStateT isDone s st = do
  (_, newState) <- runStateT st s
  if isDone newState then
    return newState
  else
    loopStateT isDone newState st

main :: IO ()
main = do
  (ps, as) <- loopStateT shouldExit (0, 0) step
  when (isWinner ps) $ putStrLn "Conguratulations, Player!"
  when (isWinner as) $ putStrLn "Conguratulations, AI!"
  return ()
  where
    isWinner = (>= 3)
    shouldExit (ps, as) = isWinner ps || isWinner as
