{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game where

import Control.Monad.State
import Text.Read (readMaybe)

data Player = Player1 | Player2
  deriving (Show, Eq)

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

newtype GameMonad a = GameMonad
  { gameAction :: StateT (Player, Int) IO a
  } deriving (Functor, Applicative, Monad)

playGame :: GameMonad Player
playGame = do
  promptPlayer
  input <- readInput
  validateResult <- validateMove input
  case validateResult of
    Nothing -> playGame
    Just i -> do
      (currentPlayer, currentVal) <- GameMonad get
      let nextVal = currentVal + i
      if nextVal == 100
        then return currentPlayer
        else GameMonad (put (nextPlayer currentPlayer, nextVal)) >> playGame

validateMove :: String -> GameMonad (Maybe Int)
validateMove input = case readMaybe input :: Maybe Int of
  Nothing -> logMessage "Invalid move, cannot read as integer" >> return Nothing
  Just i -> do
    currentVal <- GameMonad $ gets snd
    if i > 10 || i < 0
      then logMessage "Invalid move, cannot be > 10 or < 0" >> return Nothing
      else if currentVal + i > 100
        then logMessage "Invalid move, total cannot exceed 100" >> return Nothing
        else return (Just i)

promptPlayer :: GameMonad ()
promptPlayer = do
  (currentPlayer, currentVal) <- GameMonad get
  logMessage $ "Current Value: " ++ show currentVal
  logMessage $ show currentPlayer ++ "'s move."

logMessage :: String -> GameMonad ()
logMessage = GameMonad . lift . putStrLn

readInput :: GameMonad String
readInput = GameMonad $ lift $ getLine

runGame :: IO ()
runGame = do
  winningPlayer <- evalStateT (gameAction playGame) (Player1, 0)
  putStrLn $ show winningPlayer ++ " wins!"
