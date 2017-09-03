{-# LANGUAGE NoImplicitPrelude                #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Actor
import           Actor.Controller
import           BasePrelude hiding (group)
import           Control.FRPNow.Time (delayTime)
import qualified Data.Map as M
import           Game.Sequoia
import           Game.Sequoia.Keyboard
import           GameState
import           Level.Level
import           Linear.Vector
import           Math (clamp')
import           Object
import           Types (geometry)

gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600


render :: GameState -> Element
render state = collage gameWidth gameHeight
             . pure
             . move (center - cam)
             . group
             $ fmap drawLine (geometry level)
            ++ forms level
            ++ fmap renderObject (M.elems $ _objects level)
            ++ drawPlayer (_player state)
    where cam    = clampCamera (levelSize $ _currentLevel state) $ _camera state
          center = V2 (fromIntegral gameWidth) (fromIntegral gameHeight) ^* 0.5
          level = _currentLevel state


clampCamera :: V2  -- ^ Level size.
            -> V2  -- ^ Focal point.
            -> V2
clampCamera (V2 rx ry)
            (V2 x y) = result
  where
    sx = fromIntegral gameWidth
    sy = fromIntegral gameHeight
    w = sx / 2
    h = sy / 2
    result = V2 (clamp' w (rx - w) x) (clamp' h (ry - h) y)



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
    config = EngineConfig (gameWidth, gameHeight) "IWMAG" $ rgb 0.8 0.8 0.8

