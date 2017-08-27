{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude hiding (group)
import Control.FRPNow.Time (delayTime)
import Game.Sequoia
import Game.Sequoia.Keyboard
import GameState
import Level.Level
import Linear.Vector
import Object
import Player
import Player.Controller

gameWidth :: Int
gameWidth = 600

gameHeight :: Int
gameHeight = 400


render :: GameState -> Element
render state = collage gameWidth gameHeight
             . pure
             . move (center - cam)
             . group
             $ drawPlayer (player state)
               ++ forms level
               ++ fmap renderObject (objects level)
    where cam    = camera state
          center = V2 (fromIntegral gameWidth) (fromIntegral gameHeight) ^* 0.5
          level = currentLevel state


runGame :: Engine -> N (B Element)
runGame _ = do
  clock      <- getClock
  rController <- getKeyboard
  rOldCtrller <- sample $ delayTime (deltaTime clock) [] rController

  (gameAndCtrl, _) <- foldmp (initState, initController) $ \(g, ctrl) -> do
    dt   <- sample $ deltaTime clock
    rctrl <- sample $ ctrlSignal rOldCtrller rController
    let ctrl' = foldController dt rctrl ctrl
    pure $ (update dt ctrl' g, ctrl')

  return $ do
    (game', _) <- sample gameAndCtrl
    pure $ render game'


main :: IO ()
main = play config runGame pure
  where
    config = EngineConfig (gameWidth, gameHeight) "IWMAG" $ rgb 0 0 0

