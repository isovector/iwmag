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
import Linear.Metric
import Linear.Vector
import Math
import Player.Constants
import Player.Controller
import Player.Data
import Player.JumpState


getGraspHook :: Level -> Player -> Maybe Hook
getGraspHook l p = getFirst
                   . mconcat
                   . fmap intersectsWithPlayer
                   $ targets l
  where
    intersectsWithPlayer t = First
                           . bool Nothing (Just t)
                           . withinRadius playerGeom
                                          (pPos p)
                                          targetRadius
                           $ hookPos t


isStanding :: Player -> Bool
isStanding = isStand . jumpState

isBoosting :: Player -> Bool
isBoosting p = let state = jumpState p
                in isBoost state

isGrasping :: Player -> Bool
isGrasping p = case attachment p of
                 Grasping _ _ -> True
                 _            -> False

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go _           = True

collision :: Level -> Axis -> V2 -> Double -> (Maybe Line, V2)
collision l ax pos dx = sweep playerGeom pos (geometry l) ax dx

jumpHandler :: Time -> Level -> Controller -> Player -> Player
jumpHandler dt l ctrl p = bool (go $ jumpState p) p $ isGrasping p
  where
    go (Stand)  = p

    go (Jump y) =
      case collided of
        Just line ->
          let landed = onLandHandler p
           in landed { pPos =  pos'
                     , attachment = StandingOn line
                     }
        Nothing ->
          p { jumpState = Jump $ y + (gravity') * dt
            , pPos = pos'
            }
      where
          gravity' = if ctrlJump ctrl && y < 0
                        then gravity * jumpAttenuation
                        else gravity
          (collided, pos') = collision l AxisY (pPos p) $ dt * y

    go (Boost dir t)
        | t > 0 = p { jumpState = Boost dir $ t - dt
                    , pPos      = xy'
                    }
        | otherwise = addRecovery $ setFalling p
      where (_, x')  = collision l AxisX (pPos p) $ view _x boostDt
            (_, xy') = collision l AxisY x' $ view _y boostDt
            boostDt = (dt * boostStrength) *^ dir

setBoosting :: V2 -> Player -> Player
setBoosting dir p = p
  { jumpState  = Boost dir boostTime
  , boostsLeft = boostsLeft p - 1
  , attachment = Unattached
  }

onLandHandler :: Player -> Player
onLandHandler p = p
  { jumpState  = Stand
  , jumpsLeft  = jumpCount
  , boostsLeft = boostCount
  }

canBoost :: Level -> Player -> Bool
canBoost (Level{noBoostZones = zs}) p = boostsLeft p > 0
                                     && recoveryTime p == 0
                                     && not (flip any zs $ flip inRect (pPos p))

doJump :: Player -> Player
doJump p = p
  { jumpState  = Jump (-jumpStrength)
  , jumpsLeft  = jumpsLeft p - 1
  , attachment = Unattached
  }

actionHandler :: Level -> Controller -> Player -> Player
actionHandler l ctrl p
    | not (canAct p) = p
    | shouldBoost    = setBoosting (fromJust $ wantsBoost ctrl) p
    | shouldJump     = doJump p
    | wantsGrasp ctrl =
      case getGraspHook l p of
        Just t  -> onLandHandler p
                   { attachment = Grasping t
                                . normalize
                                . set _y 0
                                $ pPos p - hookPos t
                   }
        Nothing -> p
    | otherwise       = p
  where
    shouldBoost = isJust (wantsBoost ctrl)
               && not (isBoosting p)
               && not (isGrasping p)
               && canBoost l p
    shouldJump  = wantsJump ctrl
               && jumpsLeft p > 0

setFalling :: Player -> Player
setFalling p =
  p { jumpState  = Jump 0
    , attachment = Unattached
    }

stillStanding :: Player -> Bool
stillStanding p =
  case attachment p of
    Unattached -> False
    StandingOn l -> isJust . fst $ sweep playerGeom (pPos p) [l] AxisY 1
    Grasping _ _ -> True

recoveryHandler :: Time -> Player -> Player
recoveryHandler dt p = p { recoveryTime = max 0 $ recoveryTime p - dt }

fallHandler :: Player -> Player
fallHandler p
  | isStanding p =
    if stillStanding p
       then p
       else setFalling p
  | otherwise    = p

addRecovery :: Player -> Player
addRecovery p = p { recoveryTime = recoverTime }

walkHandler :: Time -> Level -> Controller -> Player -> Player
walkHandler dt l ctrl p
    | isGrasping p = p
    | canAct p     = p { pPos = pos' }
    | otherwise    = p
  where (_, pos') = collision l AxisX (pPos p) $ walkSpeed * dt * dir
        dir       = view _x . ctrlDir $ ctrl

deathHandler :: Level -> Player -> Maybe Player
deathHandler l p = if any (flip inRect (pPos p)) $ deathZones l
                      then Nothing
                      else Just p

graspHandler :: Controller -> Player -> Player
graspHandler ctrl p =
  case attachment p of
    Grasping t dir -> case (wantsJump ctrl, wantsGrasp ctrl) of
      (False, False) ->
        let dir' = if ctrlDir ctrl /= V2 0 0
                      then ctrlDir ctrl
                      else dir
        in p { attachment = Grasping t dir'
             , pPos = hookPos t
                    + dir' ^* targetRadius
                    + onSideways
                        (V2 0 0)
                        ((V2 0 0.5) ^* topY playerGeom)
                        dir'
             }
      (False, True) -> setBoosting (boostDir dir) p
      (True, False) -> doJump p { jumpsLeft = 0 }
      _ -> p
    _ -> p
  where
    onSideways a b dir = bool a b $ view _x dir /= 0
    boostDir dir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir


playerHandler :: Time -> Level -> Controller -> Player -> Maybe Player
playerHandler dt l ctrl p = deathHandler l
                          . fallHandler
                          . jumpHandler dt l ctrl
                          . actionHandler l ctrl
                          . graspHandler ctrl
                          . recoveryHandler dt
                          . walkHandler dt l ctrl
                          $ p

