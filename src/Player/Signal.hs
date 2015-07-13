module Player.Signal ( Player
                     , pPos
                     , jumpState
                     , playerHandler
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

collision :: Axis -> Vector2 -> Double -> (Maybe Line, Vector2)
collision ax pos dx = sweep playerGeom pos (geometry defaultLevel) ax dx

jumpHandler :: Controller -> Player -> Player
jumpHandler ctrl p = go $ jumpState p
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
              gravity' = if ctrlJump ctrl && y < 0
                            then gravity * jumpAttenuation
                            else gravity
              (collided, pos') = collision AxisY (pPos p) $ dt * y

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = p { jumpState = Boost (vnormalise . ctrlDir $ ctrl) boostTime }

        go (Boost dir t)
            | t > 0     = p { jumpState = Boost dir (t - dt)
                            , pPos = xy'
                            }
            | otherwise = setFalling p
          where (_, x')  = collision AxisX (pPos p) $ v2x boostDt
                (_, xy') = collision AxisY x' $ v2y boostDt
                boostDt = (dt * boostStrength) |* dir

onLandHandler :: Player -> Player
onLandHandler p = p { jumpState  = Stand
                    , jumpsLeft  = jumpCount
                    , hasBoosted = False
                    }

actionHandler :: Controller -> Player -> Player
actionHandler ctrl p
    | not (canAct p) = p
    | shouldBoost    = p { jumpState  = Prepare prepareTime
                         , jumpsLeft  = 0
                         , hasBoosted = True
                         , standingOn = Nothing
                         }
    | shouldJump     = p { jumpState  = Jump (-jumpStrength)
                         , jumpsLeft  = jumpsLeft p - 1
                         , standingOn = Nothing
                         }
    | otherwise      = p
      where shouldBoost =  wantsBoost ctrl
                        && not (isBoosting p)
                        && not (hasBoosted p)

            shouldJump  =  wantsJump ctrl
                        && jumpsLeft p > 0

setFalling :: Player -> Player
setFalling p = p { jumpState = Jump 0
                 , standingOn = Nothing
                 }

stillStanding :: Player -> Bool
stillStanding p = go $ standingOn p
  where go (Just l) = isJust . fst $ sweep playerGeom (pPos p) [l] AxisY 1
        go _        = False

fallHandler :: Player -> Player
fallHandler p
    | isStanding p = if stillStanding p
                        then p
                        else setFalling p
    | otherwise    = p

walkHandler :: Controller -> Player -> Player
walkHandler ctrl p
    | canAct p  = p { pPos = pos' }
    | otherwise = p
      where (_, pos') = collision AxisX (pPos p) $ walkSpeed * dt * dir
            dir = v2x . ctrlDir $ ctrl

deathHandler :: Level -> Player -> Maybe Player
deathHandler l p = if any (flip inRect (pPos p)) $ deathZones l
                      then Nothing
                      else Just p


playerHandler :: Level -> Controller -> Player -> Maybe Player
playerHandler l ctrl p = deathHandler l
                       . fallHandler
                       . jumpHandler ctrl
                       . actionHandler ctrl
                       . walkHandler ctrl
                       $ p

