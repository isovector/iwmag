module Player ( Controller
              , Player
              , ctrlSignal
              , playerSignal
              , drawPlayer
              , initialCtrl
              ) where

import Math
import Timing
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Keyboard as Keyboard

data Controller =
    Controller { ctrlDir   :: Vector2
               , ctrlJump  :: Bool
               , ctrlBoost :: Bool
               } deriving (Show)

data Player =
    Player { pPos :: Vector2
           }

drawPlayer :: Player -> Form
drawPlayer player = move (toPair . pPos $ player)
                  $ filled white
                  $ rect 40 40

initialCtrl :: Controller
initialCtrl =
    Controller { ctrlDir = Vector2 0 0
               , ctrlJump = False
               , ctrlBoost = False
               }

ctrlSignal :: Signal Controller
ctrlSignal = signal
  where
      makeState (x, y) jump boost =
          Controller { ctrlDir = Vector2 (fromIntegral x) (fromIntegral y)
                     , ctrlJump = jump
                     , ctrlBoost = boost
                     }

      signal   = makeState <~ Keyboard.arrows ~~ jumpKey ~~ boostKey
      jumpKey  = Keyboard.isDown Keyboard.LeftShiftKey
      boostKey = Keyboard.isDown Keyboard.ZKey

playerSignal :: Signal Player
playerSignal = foldu go initialState initialCtrl ctrlSignal
  where
      initialState = Player { pPos = Vector2 100 100 }
      go dt ctrl state =
          let speed = 100
           in state { pPos = speed * dt |* ctrlDir ctrl + pPos state
                    }

