{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Actor.Signal where

import           Actor.Constants
import Control.Monad.Trans.Reader (runReaderT)
import           Actor.JumpState
import           Collision
import           Control.Monad.State (State, modify, get, gets, execState, put)
import qualified Data.Map as M
import           Game.Sequoia
import           Linear.Metric
import           Linear.Vector
import           Math
import           Object
import           Types hiding (grasp)

sweep' :: BoxGeom -> V2 -> [Piece] -> Axis -> Double -> (Maybe Piece, V2)
sweep' b v = sweep b v pieceLine

-- getGraspHook :: Level -> Actor -> Maybe Hook
-- getGraspHook l p = getFirst
--                  . mconcat
--                  . fmap intersectsWithActor
--                  $ targets l
--   where
--     intersectsWithActor t = First
--                            . bool Nothing (Just t)
--                            . withinRadius (aGeom p)
--                                           (_aPos p)
--                                           targetRadius
--                            $ hookPos t

-- graspLevel :: GameState -> Maybe (GameState -> GameState, GraspTarget)
-- graspLevel gs = getFirst
--               . mconcat
--               . fmap f
--               . M.elems
--               . _objects
--               $ _currentLevel gs
--   where
--     f :: Object -> First (GameState -> GameState, GraspTarget)
--     f obj = First
--           . fmap (\(obj', gt, gs') ->

--               ( (currentLevel . cloneLens (objLens obj) ?~ obj') . gs'
--               , gt))
--           $ graspObject gs obj



isStanding :: Actor -> Bool
isStanding = isStand . _jumpState . _jumpData

isBoosting :: Actor -> Bool
isBoosting p =
  let state = _jumpState $ _jumpData p
   in isBoost state

-- isHolding :: Actor -> Bool
-- isHolding p = case graspTarget p of
--                  Holding _ _ -> True
--                  _           -> False

isGrasping :: Actor -> Bool
isGrasping p =
  case _attachment p of
    Grasping _ _ -> True
    _            -> False

canAct :: Actor -> Bool
canAct = not . isBoosting

collision :: Level -> Axis -> BoxGeom -> V2 -> Double -> (Maybe Piece, V2)
collision l ax geom pos dx = sweep' geom pos (geometry l) ax dx

-- jumpHandler :: Time -> Level -> Controller -> Actor -> (Actor, Maybe Piece)
-- jumpHandler dt l ctrl p = bool (go $ jumpState p) (p, Nothing) $ isGrasping p
--   where
--     go (Stand)  = (p, Nothing)

setBoosting :: V2 -> Bool -> Double -> Time -> Actor -> Actor
setBoosting dir penalty strength duration p =
  p & jumpData . jumpState  .~ Boost dir strength duration penalty
    & jumpData . boostsLeft -~ 1
    & attachment .~ Unattached

doLand :: Actor -> Actor
doLand p = p & jumpData . jumpState  .~ Stand
             & jumpData . jumpsLeft  .~ jumpCount
             & jumpData . boostsLeft .~ boostCount

canBoost :: Level -> Actor -> Bool
canBoost (Level{noBoostZones = zs}) p =
  p ^. jumpData . boostsLeft > 0
    && p ^. jumpData . recoveryTime == 0
    && not (flip any zs $ flip inRect (_aPos p))

doJump :: Actor -> Actor
doJump p =
  p & jumpData . jumpState .~ Jump (-jumpStrength)
    & jumpData . jumpsLeft -~ 1
    & attachment .~ Unattached

-- actionHandler :: GameState -> Controller -> Actor -> State GameState Actor
-- actionHandler gs ctrl p
--     | not (canAct p) = pure p
--     | shouldBoost    = pure $ setBoosting (fromJust $ wantsBoost ctrl) True boostStrength boostTime p
--     | shouldJump     = pure $ doJump p
--     | wantsGrasp ctrl =
--       case (graspTarget p, getGraspHook l p, graspLevel gs) of
--         (Holding _ throw, _, _) -> do
--           modify $ throw p $ ctrlDir ctrl
--           pure $ p { graspTarget = Unarmed }
--         (_, Just t, _) ->
--           pure $ onLandHandler p
--             { attachment = Grasping t
--                          . normalize
--                          . set _y 0
--                          $ _aPos p - hookPos t
--             }
--         (_, _, Just (lf, grasp)) -> do
--           modify lf
--           pure $ p { graspTarget = grasp }
--         (_, Nothing, Nothing) -> pure p
--     -- | wantsDig ctrl =
--     --   case graspLevel gs of
--     --     (Just (lf, grasp)) -> do
--     --       modify lf
--     --       pure $ p { graspTarget = grasp }
--     --     (Nothing) -> pure p
--     | otherwise       = pure p
--   where
--     l = _currentLevel gs
--     -- TODO(sandy): rename grasp to cling for hooks
--     shouldBoost = isJust (wantsBoost ctrl)
--                && not (isBoosting p)
--                && not (isGrasping p)
--                && canBoost l p
--     shouldJump  = wantsJump ctrl
--                && jumpsLeft p > 0

setFalling :: Actor -> Actor
setFalling p = p & jumpData . jumpState .~ Jump 0
                 & attachment .~ Unattached

stillStanding :: GameState -> Actor -> Bool
stillStanding gs p =
  case _attachment p of
    Unattached -> False
    StandingOn l ->
      isJust . fst
             $ sweep' (aGeom p)
                      (_aPos p)
                      (bool [l] (geometry $ _currentLevel gs) $ _geometryChanged gs)
                      AxisY
                      1
    Grasping _ _ -> True

doRecovety :: Time -> Actor -> Actor
doRecovety dt p =
  p & jumpData . recoveryTime .~ max 0 (p ^. jumpData . recoveryTime - dt)

addRecovery :: Actor -> Actor
addRecovery p = p & jumpData . recoveryTime .~ recoverTime

-- deathHandler :: Level -> Actor -> Maybe Actor
-- deathHandler l p =
--   if (any (flip inRect (_aPos p)) $ deathZones l) || _aHealth p <= 0
--      then Nothing
--                       else Just p

-- graspHandler :: Controller -> Actor -> Actor
-- graspHandler ctrl p =
--   case attachment p of
--     Grasping t dir -> case (wantsJump ctrl, wantsGrasp ctrl) of
--       (False, False) ->
--         let dir' = if ctrlDir ctrl /= V2 0 0
--                       then ctrlDir ctrl
--                       else dir
--         in p { attachment = Grasping t dir'
--               , _aPos = hookPos t
--                      + dir' ^* targetRadius
--                      + onSideways
--                          (V2 0 0)
--                          ((V2 0 0.5) ^* topY (aGeom p))
--                          dir'
--               }
--       (False, True) -> setBoosting (boostDir dir) False boostStrength boostTime p
--       (True, False) -> doJump p { jumpsLeft = 0 }
--       _ -> p
--     _ -> p
--   where
--     onSideways a b dir = bool a b $ view _x dir /= 0
--     boostDir dir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir

-- holdHandler :: Time -> Actor -> State GameState Actor
-- holdHandler dt p = do
--   case graspTarget p of
--     Unarmed     -> pure ()
--     Holding f _ -> modify $ f dt p
--   pure p



defaultStartJumpHandler :: Handler ()
defaultStartJumpHandler = do
  p <- gets $ view hctxPlayer

  when (p ^. jumpData . jumpsLeft > 0) $ do
    hctxPlayer %= doJump


defaultStartBoostHandler :: Handler ()
defaultStartBoostHandler = do
  p    <- gets $ view hctxPlayer
  l    <- gets $ view hctxLevel
  ctrl <- asks hctxController

  when (canBoost l p) $ do
    hctxPlayer %= setBoosting (fromJust $ wantsBoost ctrl)
                              True
                              boostStrength
                              boostTime


runHandlers :: Time -> Controller -> Actor -> State GameState Actor
runHandlers dt ctrl a = swizzle a
                      . flip runReaderT (HContext dt ctrl)
                      $ do
  hctxPlayer %= doRecovety dt
  walkHandler

  numJumps <- gets . view $ hctxPlayer . jumpData . jumpsLeft
  when (wantsJump ctrl && numJumps > 0) $ do
    startJumpHandler

  p <- gets $ view hctxPlayer
  when (isJust (wantsBoost ctrl) && not (isBoosting p) && not (isGrasping p)) $ do
    startBoostHandler

  let doCollide =
        \case
          Just piece -> collideHandler piece
          Nothing    -> pure ()

  case p ^. jumpData . jumpState of
    Stand  -> standHandler
    Jump y -> jumpHandler y >>= doCollide
    Boost dir strength time applyPenalty ->
      boostHandler dir strength time applyPenalty >>= doCollide
  where
    Handlers {..} = handlers a

    swizzle :: b -> State (c, b) () -> State c b
    swizzle b s = do
      c <- get
      let (c', b') = execState s (c, b)
      put c'
      pure b'




-- -- TODO(sandy): remove the kleisli so each of these gets the level from the monad
-- playerHandler :: Time -> GameState -> Controller -> Actor -> State GameState (Maybe Actor)
-- playerHandler dt gs ctrl p
--     = k (deathHandler l)
--   =<< holdHandler dt
--   =<< k (fallHandler gs)
--   =<< k (fst <$> jumpHandler dt l ctrl)
--   =<< actionHandler gs ctrl
--   =<< k (graspHandler ctrl)
--   =<< k (doRecovety dt)
--   =<< k (walkHandler dt l ctrl)
--   =<< pure p
--   where
--     k :: Monad m => (a -> b) -> a -> m b
--     k = (pure .)


-- actionHandler :: GameState -> Controller -> Actor -> State GameState Actor
-- actionHandler gs ctrl p
--     | not (canAct p) = pure p
--     | shouldBoost    = pure $ setBoosting (fromJust $ wantsBoost ctrl) True boostStrength boostTime p
--     | wantsGrasp ctrl =
--       case (graspTarget p, getGraspHook l p, graspLevel gs) of
--         (Holding _ throw, _, _) -> do
--           modify $ throw p $ ctrlDir ctrl
--           pure $ p { graspTarget = Unarmed }
--         (_, Just t, _) ->
--           pure $ onLandHandler p
--             { attachment = Grasping t
--                          . normalize
--                          . set _y 0
--                          $ _aPos p - hookPos t
--             }
--         (_, _, Just (lf, grasp)) -> do
--           modify lf
--           pure $ p { graspTarget = grasp }
--         (_, Nothing, Nothing) -> pure p
--     -- | wantsDig ctrl =
--     --   case graspLevel gs of
--     --     (Just (lf, grasp)) -> do
--     --       modify lf
--     --       pure $ p { graspTarget = grasp }
--     --     (Nothing) -> pure p
--     | otherwise       = pure p
--   where
--     l = _currentLevel gs
--     -- TODO(sandy): rename grasp to cling for hooks
--     shouldBoost = isJust (wantsBoost ctrl)
--                && not (isBoosting p)
--                && not (isGrasping p)
--                && canBoost l p

defaultWalkHandler :: Handler ()
defaultWalkHandler = do
  dt   <- asks hctxTime
  ctrl <- asks hctxController
  p    <- gets $ view hctxPlayer
  l    <- gets $ view hctxLevel

  when (isGrasping p && canAct p) $ do
    let dir  = view _x . ctrlDir $ ctrl
        pos' = snd . collision l
                               AxisX
                               (aGeom p)
                               (_aPos p)
                   $ walkSpeed * dt * dir

    hctxPlayer . aPos .= pos'


defaultStandHandler :: Handler ()
defaultStandHandler = do
  gs <- gets $ view hctxState
  p  <- gets $ view hctxPlayer

  when (not $ stillStanding gs p) $ do
    hctxPlayer %= setFalling


defaultJumpHandler :: Double -> Handler (Maybe Piece)
defaultJumpHandler y = do
  dt   <- asks hctxTime
  ctrl <- asks hctxController
  p    <- gets $ view hctxPlayer
  l    <- gets $ view hctxLevel

  let gravity' =
        if ctrlJump ctrl && y < 0
           then gravity * jumpAttenuation
           else gravity
      (collided, pos') = collision l
                                   AxisY
                                   (aGeom p)
                                   (_aPos p)
                                   (dt * y)

  hctxPlayer . aPos .= pos'

  case collided of
    Just line -> do
      hctxPlayer              %= doLand
      hctxPlayer . attachment .= StandingOn line

    Nothing -> do
      hctxPlayer . jumpData . jumpState .= Jump (y + (min gravity' terminalVelocity) * dt)

  pure collided



defaultBoostHandler :: V2 -> Double -> Time -> Bool -> Handler (Maybe Piece)
defaultBoostHandler _ _ t _ | t <= 0 = do
  hctxPlayer %= setFalling
  hctxPlayer %= addRecovery
  pure Nothing

defaultBoostHandler dir strength t applyPenalty = do
  dt   <- asks hctxTime
  p    <- gets $ view hctxPlayer
  l    <- gets $ view hctxLevel

  let (wx, x')  = collision l AxisX (aGeom p) (_aPos p) $ view _x boostDt
      (wy, xy') = collision l AxisY (aGeom p) x' $ view _y boostDt
      wall = wx <|> wy
      boostDt = (^* dt)
              $ strength *^ dir
              + ( gravity
                * boostAttenuation
                * bool 1
                       boostUpPenalty
                       (dir == V2 0 (-1))
                * bool 0 1 applyPenalty
                ) *^ V2 0 1

  hctxPlayer . jumpData
             . jumpState .= Boost dir
                                  strength
                                  (t - dt)
                                  applyPenalty
  hctxPlayer . aPos .= xy'

  pure wall

