{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Signal where

import Actor.Constants
import Actor.Data
import Actor.JumpState
import BasePrelude
import Collision
import Control.Lens hiding (Level)
import Game.Sequoia
import Linear.Metric
import Linear.Vector
import Math
import Types


getGraspHook :: Level -> Actor -> Maybe Hook
getGraspHook l p = getFirst
                 . mconcat
                 . fmap intersectsWithActor
                 $ targets l
  where
    intersectsWithActor t = First
                           . bool Nothing (Just t)
                           . withinRadius (aGeom p)
                                          (aPos p)
                                          targetRadius
                           $ hookPos t


isStanding :: Actor -> Bool
isStanding = isStand . jumpState

isBoosting :: Actor -> Bool
isBoosting p = let state = jumpState p
                in isBoost state

isGrasping :: Actor -> Bool
isGrasping p = case attachment p of
                 Grasping _ _ -> True
                 _            -> False

canAct :: Actor -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go _           = True

collision :: Level -> Axis -> BoxGeom -> V2 -> Double -> (Maybe Line, V2)
collision l ax geom pos dx = sweep geom pos (geometry l) ax dx

jumpHandler :: Time -> Level -> Controller -> Actor -> Actor
jumpHandler dt l ctrl p = bool (go $ jumpState p) p $ isGrasping p
  where
    go (Stand)  = p

    go (Jump y) =
      case collided of
        Just line ->
          let landed = onLandHandler p
           in landed { aPos =  pos'
                     , attachment = StandingOn line
                     }
        Nothing ->
          p { jumpState = Jump $ y + (min gravity' terminalVelocity) * dt
            , aPos = pos'
            }
      where
          gravity' = if ctrlJump ctrl && y < 0
                        then gravity * jumpAttenuation
                        else gravity
          (collided, pos') = collision l AxisY (aGeom p) (aPos p) $ dt * y

    go (Boost dir t)
        | t > 0 = p { jumpState = Boost dir $ t - dt
                    , aPos      = xy'
                    }
        | otherwise = addRecovery $ setFalling p
      where (_, x')  = collision l AxisX (aGeom p) (aPos p) $ view _x boostDt
            (_, xy') = collision l AxisY (aGeom p) x' $ view _y boostDt
            boostDt = (dt * boostStrength) *^ dir

setBoosting :: V2 -> Actor -> Actor
setBoosting dir p = p
  { jumpState  = Boost dir boostTime
  , boostsLeft = boostsLeft p - 1
  , attachment = Unattached
  }

onLandHandler :: Actor -> Actor
onLandHandler p = p
  { jumpState  = Stand
  , jumpsLeft  = jumpCount
  , boostsLeft = boostCount
  }

canBoost :: Level -> Actor -> Bool
canBoost (Level{noBoostZones = zs}) p = boostsLeft p > 0
                                     && recoveryTime p == 0
                                     && not (flip any zs $ flip inRect (aPos p))

doJump :: Actor -> Actor
doJump p = p
  { jumpState  = Jump (-jumpStrength)
  , jumpsLeft  = jumpsLeft p - 1
  , attachment = Unattached
  }

actionHandler :: Level -> Controller -> Actor -> Actor
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
                                $ aPos p - hookPos t
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

setFalling :: Actor -> Actor
setFalling p =
  p { jumpState  = Jump 0
    , attachment = Unattached
    }

stillStanding :: Actor -> Bool
stillStanding p =
  case attachment p of
    Unattached -> False
    StandingOn l -> isJust . fst $ sweep (aGeom p) (aPos p) [l] AxisY 1
    Grasping _ _ -> True

recoveryHandler :: Time -> Actor -> Actor
recoveryHandler dt p = p { recoveryTime = max 0 $ recoveryTime p - dt }

fallHandler :: Actor -> Actor
fallHandler p
  | isStanding p =
    if stillStanding p
       then p
       else setFalling p
  | otherwise    = p

addRecovery :: Actor -> Actor
addRecovery p = p { recoveryTime = recoverTime }

walkHandler :: Time -> Level -> Controller -> Actor -> Actor
walkHandler dt l ctrl p
    | isGrasping p = p
    | canAct p     = p { aPos = pos' }
    | otherwise    = p
  where (_, pos') = collision l AxisX (aGeom p) (aPos p) $ walkSpeed * dt * dir
        dir       = view _x . ctrlDir $ ctrl

deathHandler :: Level -> Actor -> Maybe Actor
deathHandler l p = if any (flip inRect (aPos p)) $ deathZones l
                      then Nothing
                      else Just p

graspHandler :: Controller -> Actor -> Actor
graspHandler ctrl p =
  case attachment p of
    Grasping t dir -> case (wantsJump ctrl, wantsGrasp ctrl) of
      (False, False) ->
        let dir' = if ctrlDir ctrl /= V2 0 0
                      then ctrlDir ctrl
                      else dir
        in p { attachment = Grasping t dir'
             , aPos = hookPos t
                    + dir' ^* targetRadius
                    + onSideways
                        (V2 0 0)
                        ((V2 0 0.5) ^* topY (aGeom p))
                        dir'
             }
      (False, True) -> setBoosting (boostDir dir) p
      (True, False) -> doJump p { jumpsLeft = 0 }
      _ -> p
    _ -> p
  where
    onSideways a b dir = bool a b $ view _x dir /= 0
    boostDir dir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir


playerHandler :: Time -> Level -> Controller -> Actor -> Maybe Actor
playerHandler dt l ctrl p
  = deathHandler l
  . fallHandler
  . jumpHandler dt l ctrl
  . actionHandler l ctrl
  . graspHandler ctrl
  . recoveryHandler dt
  . walkHandler dt l ctrl
  $ p

