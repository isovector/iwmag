module Main where

import Player
import Timing
import Control.Applicative ((<$>))
import FRP.Helm
import FRP.Helm.Signal
import Debug.Trace
import FRP.Helm.Sample
import qualified FRP.Helm.Window as Window

stroke = outlined $ solid red
frame = stroke $ rect 400 620

showTrace :: (Show a) => a -> a
showTrace x = trace (show x) x

render :: Player -> (Int, Int) -> Element
render player (w, h) = centeredCollage w h $ [frame, drawPlayer player]

main :: IO ()
main = run config
     $ render <~ playerSignal
              ~~ Window.dimensions
  where
    config = defaultConfig { windowTitle = "makin' a game" }
