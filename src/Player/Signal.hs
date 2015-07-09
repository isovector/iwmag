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
import Level.Level
import FRP.Helm
import FRP.Helm.Signal
import Data.Maybe (isJust, fromJust, mapMaybe)
import Safe (headMay)

import Player.Controller
import Player.Constants
import Player.Data
import Player.JumpState

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go (Prepare _) = False
        go _           = True

wantsJump :: Player -> Bool
wantsJump = ctrlJump . ctrls

wantsBoost :: Player -> Bool
wantsBoost = ctrlBoost . ctrls

isStanding :: Player -> Bool
isStanding = isStand . jumpState

isJumping :: Player -> Bool
isJumping = isJump . jumpState

isBoosting :: Player -> Bool
isBoosting p = let state = jumpState p
                in isBoost state || isPrepare state

jumpHandler :: Player -> Player
jumpHandler p = go $ jumpState p
  where go (Stand)  = p

        go (Jump y)
            | isJust collided =
                let landed = onLandHandler p
                 in landed { pPos = fromJust collided }
            | otherwise =
                p { jumpState = Jump $ y + (gravity') * dt
                  , pPos = pPos p + dy
                  }
          where
              gravity' = if wantsJump p && y < 0
                            then gravity * jumpAttenuation
                            else gravity
              collided = collision (geometry defaultLevel) $ lineRel (pPos p) dy
              dy = (dt * y) |* vector2Y

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = p { jumpState = Boost (vnormalise . ctrlDir $ ctrls p) boostTime }

        go (Boost dir t)
            | t > 0     = p { jumpState = Boost dir (t - dt)
                            , pPos = (dt * boostStrength) |* dir + pPos p
                            }
            | otherwise = p { jumpState = Jump 0 }

onLandHandler :: Player -> Player
onLandHandler p = p { jumpState = Stand
                    , hasBoosted = False
                    }

actionHandler :: Player -> Player
actionHandler p
    | not (canAct p) = p
    | shouldBoost    = p { jumpState = Prepare prepareTime
                         , hasBoosted = True }
    | shouldJump     = p { jumpState = Jump (-jumpStrength) }
    | otherwise      = p
      where shouldBoost =  wantsBoost p
                        && not (isBoosting p)
                        && not (hasBoosted p)

            shouldJump  =  wantsJump p
                        && isStanding p

walkHandler :: Player -> Player
walkHandler p
    | canAct p  = p { pPos = (walkSpeed * dt) |* flattened + pPos p }
    | otherwise = p
      where flattened = (ctrlDir $ ctrls p) { v2y = 0 }


collision :: [Line] -> Line -> Maybe Vector2
collision ls dp = fmap ((+) $ Vector2 0 (-1))  -- HACK: find a better way of hitting it
                . headMay
                . mapMaybe (linesIntersection dp)
                $ ls

wasKeyJustPressed :: Bool -> Bool
wasKeyJustPressed b = b


shouldJump :: Controller -> Player -> Bool
shouldJump c p =  ( jumpState p == Stand
               || (isJump $ jumpState p))
               && wasKeyJustPressed (ctrlJump c)


playerSignal :: Signal Player
playerSignal = foldu go defaultPlayer noCtrls ctrlSignal
  where
      go ctrl p = jumpHandler
                . actionHandler
                . walkHandler
                $ p { ctrls = ctrl }

