module Player ( Player
              , drawPlayer
              , playerSignal
              ) where

import Player.Data
import Player.Signal
import Math
import FRP.Helm

drawPlayer :: Player -> Form
drawPlayer p = move (toPair . pPos $ p)
             $ move (-10,-10)
             $ filled white
             $ rect 20 20

