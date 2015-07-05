module Math ( toPair
            , gravity
            , module Data.Vector
            ) where

import Data.Vector

gravity :: Double
gravity = 300

toPair :: Vector2 -> (Double, Double)
toPair v = (v2x v, v2y v)

