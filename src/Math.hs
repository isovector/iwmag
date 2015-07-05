module Math ( toPair
            , module Data.Vector
            ) where

import Data.Vector

toPair :: Vector2 -> (Double, Double)
toPair v = (v2x v, v2y v)

