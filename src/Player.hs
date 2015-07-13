module Player ( Player
              , drawPlayer
              , playerHandler
              ) where

import ClassyPrelude
import Collision
import Player.Constants
import Player.Data
import Player.Signal
import Math
import FRP.Helm

drawPlayer :: Player -> [Form]
drawPlayer p = [ move (toPair pos)
                 $ move ((r - l) / 2, (b - t) / 2)
                 $ filled white
                 $ rect width height
               ]
  where pos   = pPos $ p
        l  = leftX   playerGeom
        r  = rightX  playerGeom
        t  = topY    playerGeom
        b  = bottomY playerGeom
        width  = l + r
        height = t + b

