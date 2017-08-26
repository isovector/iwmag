{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude hiding (group)
import Game.Sequoia
import Game.Sequoia.Keyboard
import Player.Controller
import GameState
import Level.Level
import Player
import Linear.Vector


gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600


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

  (game, _) <-
    foldmp initState $ \g -> do
      dt   <- sample $ deltaTime clock
      ctrl <- sample $ ctrlSignal controller
      pure $ update dt g

  return $ do
    game' <- sample game
    pure $ render game'


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (gameWidth, gameHeight) "IWMAG" $ rgb 0 0 0

