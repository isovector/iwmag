module Player.Controller ( Controller
                         , ctrlDir
                         , ctrlJump
                         , ctrlBoost
                         , ctrlSignal
                         , noCtrls
                         ) where

import ClassyPrelude
import Math
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Keyboard as Keyboard

data Controller =
    Controller { ctrlDir   :: Vector2
               , ctrlJump  :: Bool
               , ctrlBoost :: Bool
               } deriving (Show)

noCtrls :: Controller
noCtrls =
    Controller { ctrlDir = Vector2 0 0
               , ctrlJump = False
               , ctrlBoost = False
               }

ctrlSignal :: Signal Controller
ctrlSignal = signal
  where
      makeState (x, y) jump boost =
          Controller { ctrlDir = Vector2 (fromIntegral x) (fromIntegral y)
                     , ctrlJump  = jump
                     , ctrlBoost = boost
                     }

      signal   = makeState <~ Keyboard.arrows ~~ jumpKey ~~ boostKey
      jumpKey  = Keyboard.isDown Keyboard.LeftShiftKey
      boostKey = Keyboard.isDown Keyboard.ZKey

