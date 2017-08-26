{-# LANGUAGE NoImplicitPrelude #-}

module Player ( Player
              , drawPlayer
              , playerHandler
              ) where

import BasePrelude
import Collision
import Player.Constants
import Player.Data
import Player.Signal
import Game.Sequoia
import Game.Sequoia.Color

drawPlayer :: Player -> [Form]
drawPlayer p = [ move pos
                 . move (V2 ((r - l) / 2) $ (b - t) / 2)
                 . filled white
                 $ rect width height
               ]
  where
    pos    = pPos $ p
    l      = leftX   playerGeom
    r      = rightX  playerGeom
    t      = topY    playerGeom
    b      = bottomY playerGeom
    width  = l + r
    height = t + b

