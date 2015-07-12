module Player ( Player
              , drawPlayer
              , playerSignal
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
                 $ move (0, -10)
                 $ filled white
                 $ rect 20 20
               , move (toPair tl) $ filled red $ rect 4 4
               , move (toPair tr) $ filled red $ rect 4 4
               , move (toPair bl) $ filled red $ rect 4 4
               , move (toPair br) $ filled red $ rect 4 4
               ]
  where (tl, tr, bl, br) = corners playerGeom pos
        pos = pPos $ p

