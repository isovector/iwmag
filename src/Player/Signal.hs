module Player.Signal ( Player
                     , pPos
                     , ctrls
                     , jumpState
                     , playerSignal
                     ) where

import ClassyPrelude
import Math
import Utils
import Collision
import Timing
import Level.Level
import FRP.Helm
import FRP.Helm.Signal

import Player.Controller
import Player.Constants
import Player.Data
import Player.JumpState

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

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go (Prepare _) = False
        go _           = True

collision :: Axis -> Player -> Double -> (Maybe Line, Vector2)
collision ax p dx
    | ax == AxisX = go $ geomWalls defaultLevel
    | ax == AxisY = go $ geomFloor defaultLevel
      where go ls = sweep playerGeom (pPos p) ls ax dx

jumpHandler :: Player -> Player
jumpHandler p = go $ jumpState p
  where go (Stand)  = p

        go (Jump y)
            | isJust collided =
                let landed = onLandHandler p
                 in landed { pPos =  pos'
                           , standingOn = collided
                           }
            | otherwise =
                p { jumpState = Jump $ y + (gravity') * dt
                  , pPos = pos'
                  }
          where
              gravity' = if wantsJump p && y < 0
                            then gravity * jumpAttenuation
                            else gravity
              (collided, pos') = collision AxisY p $ dt * y

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = p { jumpState = Boost (vnormalise . ctrlDir $ ctrls p) boostTime }

        go (Boost dir t)
            | t > 0     = p { jumpState = Boost dir (t - dt)
                            , pPos = (dt * boostStrength) |* dir + pPos p
                            }
            | otherwise = setFalling p

onLandHandler :: Player -> Player
onLandHandler p = p { jumpState = Stand
                    , hasBoosted = False
                    }

actionHandler :: Player -> Player
actionHandler p
    | not (canAct p) = p
    | shouldBoost    = p { jumpState = Prepare prepareTime
                         , hasBoosted = True
                         , standingOn = Nothing
                         }
    | shouldJump     = p { jumpState = Jump (-jumpStrength)
                         , standingOn = Nothing
                         }
    | otherwise      = p
      where shouldBoost =  wantsBoost p
                        && not (isBoosting p)
                        && not (hasBoosted p)

            shouldJump  =  wantsJump p
                        && isStanding p

setFalling :: Player -> Player
setFalling p = p { jumpState = Jump 0
                 , standingOn = Nothing
                 }

stillStanding :: Player -> Bool
stillStanding p = go $ standingOn p
  where go (Just l) = linesIntersect (lineRel (pPos p) vector2Y) l
        go _        = False

fallHandler :: Player -> Player
fallHandler p
    | isStanding p = if stillStanding p
                        then p
                        else setFalling p
    | otherwise    = p

walkHandler :: Player -> Player
walkHandler p
    | canAct p  = p { pPos = pos' }
    | otherwise = p
      where (_, pos') = collision AxisX p $ walkSpeed * dt * dir
            dir = v2x . ctrlDir . ctrls $ p



wasKeyJustPressed :: Bool -> Bool
wasKeyJustPressed b = b


shouldJump :: Controller -> Player -> Bool
shouldJump c p =  ( jumpState p == Stand
               || (isJump $ jumpState p))
               && wasKeyJustPressed (ctrlJump c)


playerSignal :: Signal Player
playerSignal = foldu go defaultPlayer noCtrls ctrlSignal
  where
      go ctrl p = fallHandler
                . jumpHandler
                . actionHandler
                . walkHandler
                $ p { ctrls = ctrl }

