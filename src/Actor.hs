{-# LANGUAGE NoImplicitPrelude   #-}

module Actor
  ( Actor
  , drawActor
  , playerHandler
  ) where

import Actor.Data
import Actor.JumpState
import Actor.Signal
import BasePrelude
import Game.Sequoia
import Game.Sequoia.Color
import Linear.Vector
import Types

drawActor :: Actor -> [Form]
drawActor p = fmap (move pos) $
  [ move (V2 ((r - l) / 2) $ (b - t) / 2)
      . filled (aColor p)
      $ rect width height
  ]
  ++
  [ move (dir ^* (1.1 * fromIntegral (-n * (n + 3))))
      . filled (rgba 1 color 0 color)
      . circle
      $ 7 * color
  | dir <- maybeToList $ getBoostDir p
  , n   <- [0 :: Int .. 5]
  , let color = 1 - fromIntegral n / 6
  ]
  where
    pos    = aPos $ p
    l      = leftX   (aGeom p)
    r      = rightX  (aGeom p)
    t      = topY    (aGeom p)
    b      = bottomY (aGeom p)
    width  = l + r
    height = t + b

getBoostDir :: Actor -> Maybe V2
getBoostDir p =
  case jumpState p of
    Boost dir _ -> Just dir
    _           -> Nothing

