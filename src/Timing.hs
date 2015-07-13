module Timing ( frameRate
              , dt
              ) where

import ClassyPrelude
import FRP.Helm.Signal
import FRP.Helm.Sample
import qualified FRP.Helm.Time as Time

framesPerSecond :: Double
framesPerSecond = 60

dt :: Double
dt = 1 / framesPerSecond

frameRate :: Signal Time.Time
frameRate = Time.fps framesPerSecond

