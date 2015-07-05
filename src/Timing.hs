module Timing ( sampleOn
              , frameRate
              , foldu
              , dt
              ) where

import Control.Applicative ((<$>))
import FRP.Helm.Signal
import FRP.Helm.Sample
import qualified FRP.Helm.Time as Time

framesPerSecond :: Double
framesPerSecond = 60

dt :: Double
dt = 1 / framesPerSecond

frameRate :: Signal Time.Time
frameRate = Time.fps framesPerSecond

sampleOn :: Signal Time.Time -> a -> Signal a -> Signal (Double, a)
sampleOn t d s = asElapsed <$> foldp update' (Unchanged 0, d) combined
  where combined = (,) <~ (Time.inSeconds <$> t) ~~ s
        asElapsed ((Changed dt'),  b) = (dt', b)
        asElapsed ((Unchanged _), b)  = (0,  b)
        update' (a', b) (a, _) =
            if a' == value a
               then (Unchanged $ value a, b)
               else (Changed a', b)

foldu :: (a -> b -> b) -> b -> a -> Signal a -> Signal b
foldu f b0 a0 s = foldp go b0 $ sampleOn frameRate a0 s
  where go (0, _) x = x
        go (_, x') x = f x' x

