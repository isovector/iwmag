{-# LANGUAGE NoImplicitPrelude   #-}

module Player
  ( Player
  , drawPlayer
  , playerHandler
  ) where

import BasePrelude
import Collision
import Game.Sequoia
import Game.Sequoia.Color
import Linear.Vector
import Player.Constants
import Player.Data
import Player.JumpState
import Player.Signal

drawPlayer :: Player -> [Form]
drawPlayer p = fmap (move pos) $
  [ move (V2 ((r - l) / 2) $ (b - t) / 2)
      . filled white
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
    pos    = pPos $ p
    l      = leftX   playerGeom
    r      = rightX  playerGeom
    t      = topY    playerGeom
    b      = bottomY playerGeom
    width  = l + r
    height = t + b

getBoostDir :: Player -> Maybe V2
getBoostDir p =
  case jumpState p of
    Boost dir _ -> Just dir
    _           -> Nothing

