module Main where

import ClassyPrelude hiding (Element)
import Player
import Level.Level
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Window as Window
import GameState

render :: Player -> (Int, Int) -> Element
render p (w, h) = collage w h $ drawPlayer p ++ forms defaultLevel

main :: IO ()
main = run config
     $ render <~ playerSignal
              ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "makin' a game" }
