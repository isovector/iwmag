{-# LANGUAGE NoImplicitPrelude #-}

module Player.Signal ( Player
                     , pPos
                     , jumpState
                     , playerHandler
                     ) where

import BasePrelude
import Collision
import Control.Lens hiding (Level)
import Game.Sequoia
import Level.Level
import Linear.Vector
import Math
import Player.Constants
import Player.Controller
import Player.Data
import Player.JumpState


isStanding :: Player -> Bool
isStanding = isStand . jumpState

isBoosting :: Player -> Bool
isBoosting p = let state = jumpState p
                in isBoost state

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go _           = True

collision :: Level -> Axis -> V2 -> Double -> (Maybe Line, V2)
collision l ax pos dx = sweep playerGeom pos (geometry l) ax dx

jumpHandler :: Time -> Level -> Controller -> Player -> Player
jumpHandler dt l ctrl p = go $ jumpState p
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

        go (Boost dir t)
            | t > 0           = p { jumpState = Boost dir (t - dt)
                                  , pPos = xy'
                                  }
            | otherwise       = addRecovery $ setFalling p
          where (_, x')  = collision l AxisX (pPos p) $ view _x boostDt
                (_, xy') = collision l AxisY x' $ view _y boostDt
                boostDt = (dt * boostStrength) *^ dir


setBoosting :: Controller -> Player -> Player
setBoosting ctrl p = p { jumpState  = Boost (fromJust $ wantsBoost ctrl) boostTime
                       , boostsLeft = boostsLeft p - 1
                       }


onLandHandler :: Player -> Player
onLandHandler p = p { jumpState  = Stand
                    , jumpsLeft  = jumpCount
                    , boostsLeft = boostCount
                    }

canBoost :: Level -> Player -> Bool
canBoost (Level{noBoostZones = zs}) p = boostsLeft p > 0
                                     && recoveryTime p == 0
                                     && not (flip any zs $ flip inRect (pPos p))

actionHandler :: Level -> Controller -> Player -> Player
actionHandler l ctrl p
    | not (canAct p) = p
    | shouldBoost    = setBoosting ctrl p
    | shouldJump     = p { jumpState  = Jump (-jumpStrength)
                         , jumpsLeft  = jumpsLeft p - 1
                         , standingOn = Nothing
                         }
    | otherwise      = p
      where shouldBoost =  isJust (wantsBoost ctrl)
                        && not (isBoosting p)
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

recoveryHandler :: Time -> Player -> Player
recoveryHandler dt p = p { recoveryTime = max 0 $ recoveryTime p - dt }

fallHandler :: Player -> Player
fallHandler p
    | isStanding p = if stillStanding p
                        then p
                        else setFalling p
    | otherwise    = p

addRecovery :: Player -> Player
addRecovery p = p { recoveryTime = recoverTime }

walkHandler :: Time -> Level -> Controller -> Player -> Player
walkHandler dt l ctrl p
    | canAct p  = p { pPos = pos' }
    | otherwise = p
      where (_, pos') = collision l AxisX (pPos p) $ walkSpeed * dt * dir
            dir = view _x . ctrlDir $ ctrl

deathHandler :: Level -> Player -> Maybe Player
deathHandler l p = if any (flip inRect (pPos p)) $ deathZones l
                      then Nothing
                      else Just p


playerHandler :: Time -> Level -> Controller -> Player -> Maybe Player
playerHandler dt l ctrl p = deathHandler l
                          . fallHandler
                          . jumpHandler dt l ctrl
                          . actionHandler l ctrl
                          . recoveryHandler dt
                          . walkHandler dt l ctrl
                          $ p

