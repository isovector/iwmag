module Player.Signal ( Player
                     , pPos
                     , ctrls
                     , jumpState
                     , playerSignal
                     ) where

import Control.Applicative ((<$>))
import Math
import Utils
import Timing
import FRP.Helm
import FRP.Helm.Signal
import Data.Maybe (isJust, fromJust)

import Player.Controller
import Player.Constants
import Player.Data
import Player.JumpState

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go (Prepare _) = False
        go _           = True


jumpHandler :: Bool -> Player -> Player
jumpHandler isJumping p = go $ jumpState p
  where go (Stand)  = p

        go (Jump y)
            | isJust collided =
                let landed = onLandHandler p
                 in landed { pPos = fromJust collided }
            | otherwise =
                p { jumpState = Jump $ y + (gravity') * dt
                  , pPos = (dt * y) |* vector2Y  + pPos p
                  }
          where
              gravity' = if isJumping && y < 0
                            then gravity * jumpAttenuation
                            else gravity
              collided = collision $ pPos p

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = p { jumpState = Boost (vnormalise . ctrlDir $ ctrls p) boostTime }

        go (Boost dir t)
            | t > 0     = p { jumpState = Boost dir (t - dt)
                            , pPos = (dt * boostStrength) |* dir + pPos p
                            }
            | otherwise = p { jumpState = Jump 0 }

onLandHandler :: Player -> Player
onLandHandler p = p { jumpState = Stand }

actionHandler :: Player -> Player
actionHandler p
    | shouldBoost = p { jumpState = Prepare prepareTime }
    | shouldJump  = p { jumpState = Jump (-jumpStrength) }
    | otherwise   = p
      where shouldBoost = False
            shouldJump  = False

collision :: Vector2 -> Maybe Vector2
collision v = if v2y v > height
                 then Just $ v { v2y = height }
                 else Nothing
  where height = 300

wasKeyJustPressed :: Bool -> Bool
wasKeyJustPressed b = b


shouldJump :: Controller -> Player -> Bool
shouldJump c p =  ( jumpState p == Stand
               || (isJump $ jumpState p))
               && wasKeyJustPressed (ctrlJump c)


playerSignal :: Signal Player
playerSignal = foldu go defaultPlayer noCtrls ctrlSignal
  where
      go ctrl p =
          let speed = 200
              ctrls =
                  if canAct p
                     then ctrl
                     else noCtrls
              isJumping = shouldJump ctrls p
              jumpState' =
                  if isJumping
                     then Prepare prepareTime
                     else jumpState p

              flattened = (ctrlDir ctrls) { v2y = 0 }
           in jumpHandler (ctrlJump ctrl)
                $ p { pPos = speed * dt |* flattened + pPos p
                    , ctrls = ctrl
                    , jumpState = jumpState'
                    }

