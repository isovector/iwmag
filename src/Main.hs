{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude hiding (group)
import Game.Sequoia
import Game.Sequoia.Keyboard
import Player.Controller
import GameState
import Level.Level
import Control.FRPNow.Time (delayTime)
import Player
import Linear.Vector


gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 800


render :: GameState -> Element
render state = collage gameWidth gameHeight
             . pure
             . move (cam + center)
             . group
             $ drawPlayer (player state) ++ (forms $ currentLevel state)
    where cam    = negate $ camera state
          center = V2 (fromIntegral gameWidth) 0 ^* 0.5


runGame :: Engine -> N (B Element)
runGame _ = do
  clock      <- getClock
  controller <- getKeyboard
  oldCtrller <- sample $ delayTime (deltaTime clock) [] controller

  (game, _) <- foldmp initState $ \g -> do
    dt   <- sample $ deltaTime clock
    ctrl <- sample $ ctrlSignal oldCtrller controller
    pure $ update dt ctrl g

  return $ do
    game' <- sample game
    pure $ render game'


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (gameWidth, gameHeight) "IWMAG" $ rgb 0 0 0

