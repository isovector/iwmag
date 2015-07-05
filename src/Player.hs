module Player ( Controller
              , Player
              , ctrlSignal
              , playerSignal
              , drawPlayer
              ) where

import Control.Applicative ((<$>))
import Math
import Utils
import Timing
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Keyboard as Keyboard

data Controller =
    Controller { ctrlDir   :: Vector2
               , ctrlJump  :: Sample Bool
               , ctrlBoost :: Sample Bool
               } deriving (Show)

data Player =
    Player { pPos :: Vector2
           , jumpState :: JumpState
           } deriving (Show)

data JumpState = Stand | Jump Double | Boost Vector2 Double deriving (Show)



noCtrls :: Controller
noCtrls =
    Controller { ctrlDir = Vector2 0 0
               , ctrlJump = Unchanged False
               , ctrlBoost = Unchanged False
               }

ctrlSignal :: Signal Controller
ctrlSignal = foldp diffState noCtrls signal
  where
      makeState (x, y) jump boost =
          Controller { ctrlDir = Vector2 (fromIntegral x) (fromIntegral y)
                     , ctrlJump  = Unchanged jump
                     , ctrlBoost = Unchanged boost
                     }

      diffState state' state =
          state' { ctrlJump  = diff ctrlJump
                 , ctrlBoost = diff ctrlBoost
                 }
        where diff f = diff' (f state') $ f state
              diff' a' a =
                  let v = value a'
                   in if v /= value a
                         then showTrace $ Changed   v
                         else Unchanged v

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

wasKeyJustPressed :: Sample Bool -> Bool
wasKeyJustPressed (Changed b) = b
wasKeyJustPressed _ = False


shouldJump :: Controller -> Player -> Bool
shouldJump c p = wasKeyJustPressed $ ctrlJump c


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
              isJumping = shouldJump ctrls p
              jumpState' =
                  if isJumping
                     then showTrace $ Jump (-200)
                     else jumpState p
           in jumpHandler isJumping
                $ p { pPos = speed * dt |* ctrlDir ctrls + pPos p
                    , jumpState = jumpState'
                    }

