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

collision :: Level -> Axis -> Vector2 -> Double -> (Maybe Line, Vector2)
collision l ax pos dx = sweep playerGeom pos (geometry l) ax dx

jumpHandler :: Level -> Controller -> Player -> Player
jumpHandler l ctrl p = go $ jumpState p
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
              (collided, pos') = collision l AxisY (pPos p) $ dt * y

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = setBoosting ctrl p

        go (Boost dir t)
            | t > 0           = p { jumpState = Boost dir (t - dt)
                                  , pPos = xy' }
            | another         = setBoosting ctrl p
            | otherwise       = setFalling p
          where (_, x')  = collision l AxisX (pPos p) $ v2x boostDt
                (_, xy') = collision l AxisY x' $ v2y boostDt
                boostDt = (dt * boostStrength) |* dir
                another =  ctrlBoost ctrl
                        && okDirection ctrl dir
                        && canBoost l p

okDirection :: Controller -> Vector2 -> Bool
okDirection ctrl v1 = okDirection' . vnormalise $ ctrlDir ctrl
  where okDirection' v2 = let dot = vdot v1 v2
                              epsilon = 0.02
                           in dot < 1 - epsilon && dot >= 0 && vmag v2 > 0

setBoosting :: Controller -> Player -> Player
setBoosting ctrl p = p { jumpState  = Boost (vnormalise . ctrlDir $ ctrl) boostTime
                       , boostsLeft = boostsLeft p - 1
                       }


onLandHandler :: Player -> Player
onLandHandler p = p { jumpState  = Stand
                    , jumpsLeft  = jumpCount
                    , hasBoosted = False
                    , boostsLeft = boostCount
                    }

canBoost :: Level -> Player -> Bool
canBoost (Level{noBoostZones = zs}) p = boostsLeft p > 0
                                     && not (flip any zs $ flip inRect (pPos p))

actionHandler :: Level -> Controller -> Player -> Player
actionHandler l ctrl p
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
                        && canBoost l p
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

walkHandler :: Level -> Controller -> Player -> Player
walkHandler l ctrl p
    | canAct p  = p { pPos = pos' }
    | otherwise = p
      where (_, pos') = collision l AxisX (pPos p) $ walkSpeed * dt * dir
            dir = v2x . ctrlDir $ ctrl

deathHandler :: Level -> Player -> Maybe Player
deathHandler l p = if any (flip inRect (pPos p)) $ deathZones l
                      then Nothing
                      else Just p


playerHandler :: Level -> Controller -> Player -> Maybe Player
playerHandler l ctrl p = deathHandler l
                       . fallHandler
                       . jumpHandler l ctrl
                       . actionHandler l ctrl
                       . walkHandler l ctrl
                       $ p

