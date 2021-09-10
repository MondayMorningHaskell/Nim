{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Game where

import Control.Monad.IO.Class
import Control.Monad.State
import Text.Read (readMaybe)

data Player = Player1 | Player2
  deriving (Show, Eq)

nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

newtype GameMonad a = GameMonad
  { gameAction :: StateT (Player, Int) IO a
  } deriving (Functor, Applicative, Monad)

instance MonadIO GameMonad where
  liftIO action = GameMonad (lift action)

instance MonadState (Player, Int) GameMonad where
  get = GameMonad get
  put = GameMonad . put

class MonadTerminal m where
  logMessage :: String -> m ()
  getInputLine :: m String

instance MonadTerminal GameMonad where
  logMessage = liftIO . putStrLn
  getInputLine = liftIO getLine

playGame :: GameMonad Player
playGame = do
  promptPlayer
  input <- readInput
  validateResult <- validateMove input
  case validateResult of
    Nothing -> playGame
    Just i -> do
      (currentPlayer, currentVal) <- get
      let nextVal = currentVal + i
      if nextVal == 100
        then return currentPlayer
        else (put (nextPlayer currentPlayer, nextVal)) >> playGame

validateMove :: (MonadTerminal m, MonadState (Player, Int) m) => String -> m (Maybe Int)
validateMove input = case readMaybe input :: Maybe Int of
  Nothing -> logMessage "Invalid move, cannot read as integer" >> return Nothing
  Just i -> do
    currentVal <- gets snd
    if i > 10 || i < 0
      then logMessage "Invalid move, cannot be > 10 or < 0" >> return Nothing
      else if currentVal + i > 100
        then logMessage "Invalid move, total cannot exceed 100" >> return Nothing
        else return (Just i)

promptPlayer :: (MonadTerminal m, MonadState (Player, Int) m) => m ()
promptPlayer = do
  (currentPlayer, currentVal) <- get
  logMessage $ "Current Value: " ++ show currentVal
  logMessage $ show currentPlayer ++ "'s move."

readInput :: (MonadTerminal m) => m String
readInput = getInputLine

runGame :: IO ()
runGame = do
  winningPlayer <- evalStateT (gameAction playGame) (Player1, 0)
  putStrLn $ show winningPlayer ++ " wins!"
