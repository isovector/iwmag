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
             $ filled white
             $ rect 10 10

