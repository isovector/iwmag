module Player ( Controller
              , Player
              , ctrlSignal
              , playerSignal
              , drawPlayer
              ) where

import Control.Applicative ((<$>))
import Math
import Timing
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Keyboard as Keyboard
import Debug.Trace

showTrace :: (Show a) => a -> a
showTrace x = trace (show x) x

data Controller =
    Controller { ctrlDir   :: Vector2
               , ctrlJump  :: Bool
               , ctrlBoost :: Bool
               } deriving (Show)

data Player =
    Player { pPos :: Vector2
           , jumpState :: JumpState
           } deriving (Show)

data JumpState = Stand | Jump Double | Boost Vector2 Double deriving (Show)

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
                     , ctrlJump = jump
                     , ctrlBoost = boost
                     }

      signal   = makeState <~ Keyboard.arrows ~~ jumpKey ~~ boostKey
      jumpKey  = Keyboard.isDown Keyboard.LeftShiftKey
      boostKey = Keyboard.isDown Keyboard.ZKey

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go _           = True

drawPlayer :: Player -> Form
drawPlayer player = move (toPair . pPos $ player)
                  $ filled white
                  $ rect 40 40


jumpHandler :: Bool -> Player -> Player
jumpHandler isJumping p = go $ jumpState p
  where go (Stand)  = p
        go (Jump y) = p { jumpState = Jump $ y + gravity' * dt
                        , pPos = (dt * y) |* vector2Y  + pPos p
                        }
        go _ = showTrace p

        gravity' = if isJumping
                      then gravity * 0.8
                      else gravity


playerSignal :: Signal Player
playerSignal = foldu go initialState noCtrls ctrlSignal
  where
      initialState = Player { pPos = Vector2 100 100
                            , jumpState = Stand
                            }

      go ctrl p =
          let speed = 300
              ctrls =
                  if canAct p
                     then ctrl
                     else noCtrls
              isJumping = ctrlJump ctrls
              jumpState' =
                  if isJumping
                     then showTrace $ Jump (-200)
                     else jumpState p
           in jumpHandler isJumping
                $ p { pPos = speed * dt |* ctrlDir ctrls + pPos p
                    , jumpState = jumpState'
                    }

