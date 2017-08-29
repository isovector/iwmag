{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Signal where

import Actor.Constants
import Actor.Data
import Actor.JumpState
import Collision
import Control.Monad.State (State, modify)
import Game.Sequoia
import Linear.Metric
import Linear.Vector
import Math
import Object
import Types hiding (grasp)


getGraspHook :: Level -> Actor -> Maybe Hook
getGraspHook l p = getFirst
                 . mconcat
                 . fmap intersectsWithActor
                 $ targets l
  where
    intersectsWithActor t = First
                           . bool Nothing (Just t)
                           . withinRadius (aGeom p)
                                          (_aPos p)
                                          targetRadius
                           $ hookPos t

graspLevel :: Level -> Actor -> Maybe (Level -> Level, GraspTarget)
graspLevel l p = getFirst . mconcat . fmap f . zip [0..] $ _objects l
  where
    f :: (Int, Object) -> First (Level -> Level, GraspTarget)
    f (idx, obj) =
      let lo = objects . ix idx
       in First . fmap (first $ \obj' -> cloneTraversal lo .~ obj')
                $ graspObject lo p obj



isStanding :: Actor -> Bool
isStanding = isStand . jumpState

isBoosting :: Actor -> Bool
isBoosting p = let state = jumpState p
                in isBoost state

isHolding :: Actor -> Bool
isHolding p = case graspTarget p of
                 Holding _ _ -> True
                 _           -> False

isGrasping :: Actor -> Bool
isGrasping p = case attachment p of
                 Grasping _ _ -> True
                 _            -> False

canAct :: Actor -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _ _ ) = False
        go _              = True

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
           in landed { _aPos =  pos'
                     , attachment = StandingOn line
                     }
        Nothing ->
          p { jumpState = Jump $ y + (min gravity' terminalVelocity) * dt
            , _aPos = pos'
            }
      where
          gravity' = if ctrlJump ctrl && y < 0
                        then gravity * jumpAttenuation
                        else gravity
          (collided, pos') = collision l AxisY (aGeom p) (_aPos p) $ dt * y

    go (Boost dir t applyPenalty)
        | t > 0 = p { jumpState = Boost dir (t - dt) applyPenalty
                    , _aPos      = xy'
                    }
        | otherwise = addRecovery $ setFalling p
      where (_, x')  = collision l AxisX (aGeom p) (_aPos p) $ view _x boostDt
            (_, xy') = collision l AxisY (aGeom p) x' $ view _y boostDt
            boostDt = (^* dt)
                    $ boostStrength *^ dir
                    + ( gravity
                      * boostAttenuation
                      * bool 1
                             boostUpPenalty
                             (dir == V2 0 (-1))
                      * bool 0 1 applyPenalty
                      ) *^ V2 0 1

setBoosting :: V2 -> Bool -> Actor -> Actor
setBoosting dir penalty p = p
  { jumpState  = Boost dir boostTime penalty
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
                                     && not (flip any zs $ flip inRect (_aPos p))

doJump :: Actor -> Actor
doJump p = p
  { jumpState  = Jump (-jumpStrength)
  , jumpsLeft  = jumpsLeft p - 1
  , attachment = Unattached
  }

actionHandler :: Level -> Controller -> Actor -> State Level Actor
actionHandler l ctrl p
    | not (canAct p) = pure p
    | shouldBoost    = pure $ setBoosting (fromJust $ wantsBoost ctrl) True p
    | shouldJump     = pure $ doJump p
    | wantsGrasp ctrl =
      case (graspTarget p, getGraspHook l p, graspLevel l p) of
        (Holding _ throw, _, _) -> do
          modify $ throw p $ ctrlDir ctrl
          pure $ p { graspTarget = Unarmed }
        (_, Just t, _) ->
          pure $ onLandHandler p
            { attachment = Grasping t
                         . normalize
                         . set _y 0
                         $ _aPos p - hookPos t
            }
        (_, _, Just (lf, grasp)) -> do
          modify lf
          pure $ p { graspTarget = grasp }
        (_, Nothing, Nothing) -> pure p
    | otherwise       = pure p
  where
    -- TODO(sandy): rename grasp to cling for hooks
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
    StandingOn l -> isJust . fst $ sweep (aGeom p) (_aPos p) [l] AxisY 1
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
    | canAct p     = p { _aPos = pos' }
    | otherwise    = p
  where (_, pos') = collision l AxisX (aGeom p) (_aPos p) $ walkSpeed * dt * dir
        dir       = view _x . ctrlDir $ ctrl

deathHandler :: Level -> Actor -> Maybe Actor
deathHandler l p =
  if (any (flip inRect (_aPos p)) $ deathZones l) || _aHealth p <= 0
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
              , _aPos = hookPos t
                     + dir' ^* targetRadius
                     + onSideways
                         (V2 0 0)
                         ((V2 0 0.5) ^* topY (aGeom p))
                         dir'
              }
      (False, True) -> setBoosting (boostDir dir) False p
      (True, False) -> doJump p { jumpsLeft = 0 }
      _ -> p
    _ -> p
  where
    onSideways a b dir = bool a b $ view _x dir /= 0
    boostDir dir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir

holdHandler :: Time -> Actor -> State Level Actor
holdHandler dt p = do
  case graspTarget p of
    Unarmed     -> pure ()
    Holding f _ -> modify $ f dt p
  pure p



-- TODO(sandy): remove the kleisli so each of these gets the level from the monad
playerHandler :: Time -> Level -> Controller -> Actor -> State Level (Maybe Actor)
playerHandler dt l ctrl p
    = k (deathHandler l)
  =<< holdHandler dt
  =<< k (fallHandler)
  =<< k (jumpHandler dt l ctrl)
  =<< actionHandler l ctrl
  =<< k (graspHandler ctrl)
  =<< k (recoveryHandler dt)
  =<< k (walkHandler dt l ctrl)
  =<< pure p
  where
    k :: Monad m => (a -> b) -> a -> m b
    k = (pure .)

