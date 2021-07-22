{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Game where

import Control.Monad.Freer
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

getState :: (Member (State (Player, Int)) r) => Eff r (Player, Int)
getState = send (get :: State (Player, Int) (Player, Int))

putState :: (Member (State (Player, Int)) r) => (Player, Int) -> Eff r ()
putState = send . (put :: (Player, Int) -> State (Player, Int) ())

{-
class MonadTerminal m where
  logMessage :: String -> m ()
  getInputLine :: m String

instance MonadTerminal GameMonad where
  logMessage = liftIO . putStrLn
  getInputLine = liftIO getLine
-}

data Terminal a where
  LogMessage :: String -> Terminal ()
  GetInputLine :: Terminal String

logMessage :: (Member Terminal r) => String -> Eff r ()
logMessage = send . LogMessage

getInputLine :: (Member Terminal r) => Eff r String
getInputLine = send GetInputLine

runTerminalIO :: (Member IO r) => Eff (Terminal ': r) a -> Eff r a
runTerminalIO = runNat terminalToIO
  where
    terminalToIO :: Terminal a -> IO a
    terminalToIO (LogMessage msg) = putStrLn msg
    terminalToIO GetInputLine = getLine

transformGame :: Eff '[ Terminal, State (Player, Int), IO ] a -> IO a
transformGame = runM . (runNatS (Player1, 0) stateToIO) . runTerminalIO
  where
    stateToIO :: (Player, Int) -> State (Player, Int) a -> IO ((Player, Int), a)
    stateToIO prev act = let (a', nextState) = runState act prev in return (nextState, a')

playGame :: Eff '[ Terminal, State (Player, Int), IO ] Player
playGame = do
  promptPlayer
  input <- readInput
  validateResult <- validateMove input
  case validateResult of
    Nothing -> playGame
    Just i -> do
      (currentPlayer, currentVal) :: (Player, Int) <- getState
      let nextVal = currentVal + i
      if nextVal == 100
        then return currentPlayer
        else (putState (nextPlayer currentPlayer, nextVal)) >> playGame

validateMove :: (Member Terminal r, Member (State (Player, Int)) r) => String -> Eff r (Maybe Int)
validateMove input = case readMaybe input :: Maybe Int of
  Nothing -> logMessage "Invalid move, cannot read as integer" >> return Nothing
  Just i -> do
    currentVal <- snd <$> getState
    if i > 10 || i < 0
      then logMessage "Invalid move, cannot be > 10 or < 0" >> return Nothing
      else if currentVal + i > 100
        then logMessage "Invalid move, total cannot exceed 100" >> return Nothing
        else return (Just i)

promptPlayer :: (Member Terminal r, Member (State (Player, Int)) r) => Eff r ()
promptPlayer = do
  (currentPlayer, currentVal) <- getState
  logMessage $ "Current Value: " ++ show currentVal
  logMessage $ show currentPlayer ++ "'s move."

readInput :: (Member Terminal r) => Eff r String
readInput = getInputLine

runGame :: IO ()
runGame = do
  winningPlayer <- transformGame playGame
  putStrLn $ show winningPlayer ++ " wins!"
