module Main where

import ClassyPrelude hiding (Element)
import Player
import Level.Level
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Window as Window
import GameState

render :: GameState -> (Int, Int) -> Element
render state (w, h) = collage w h $ drawPlayer (player state) ++ forms defaultLevel

main :: IO ()
main = run config
     $ render <~ gameSignal
              ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "makin' a game" }
