module Main where

import ClassyPrelude hiding (Element, group)
import Player
import Level.Level
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Window as Window

import Math
import GameState

render :: GameState -> (Int, Int) -> Element
render state (w, h) = collage w h
                    . return
                    . move (toPair $ cam + center)
                    . group
                    $ drawPlayer (player state) ++ forms defaultLevel
    where cam    = negate $ camera state
          center = Vector2 (fromIntegral w) 0 *| 0.5

main :: IO ()
main = run config
     $ render <~ gameSignal
              ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "makin' a game" }
