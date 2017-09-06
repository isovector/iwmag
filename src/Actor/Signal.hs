{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Actor.Signal where

import           Actor.Constants
import           Actor.JumpState
import           Collision
import           Control.Monad.State (gets, runStateT)
import           Control.Monad.Trans.Reader (runReaderT, local)
import           Control.Monad.Writer (Writer)
import           Game.Sequoia
import           Linear.Metric
import           Linear.Vector
import           Math
import           Types

sweep' :: BoxGeom -> V2 -> [Piece] -> Axis -> Double -> (Maybe Piece, V2)
sweep' b v = sweep b v pieceLine


getNearbyHook :: Level -> Actor -> Maybe Hook
getNearbyHook l p = getFirst
                  . mconcat
                  . fmap intersectsWithActor
                  $ targets l
  where
    intersectsWithActor t
      = First
      . bool Nothing (Just t)
      . boxesIntersect (aGeom p)
                       (_aPos p)
                       (BoxGeom targetRadius
                                targetRadius
                                targetRadius
                                targetRadius)
      $ hookPos t


isStanding :: Actor -> Bool
isStanding = isStand . _jumpState . _jumpData


isBoosting :: Actor -> Bool
isBoosting p =
  let state = _jumpState $ _jumpData p
   in isBoost state


isHooked :: Actor -> Bool
isHooked p =
  case _attachment p of
    Grasping _ _ -> True
    _            -> False


canAct :: Actor -> Bool
canAct = not . isBoosting


collision :: Level -> Axis -> BoxGeom -> V2 -> Double -> (Maybe Piece, V2)
collision l ax geom pos dx = sweep' geom pos (geometry l) ax dx


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
    && p ^. jumpData . recoveryTime <= 0
    && not (flip any zs $ flip inRect (_aPos p))


doJump :: Actor -> Actor
doJump p =
  p & jumpData . jumpState .~ Jump (-jumpStrength)
    & jumpData . jumpsLeft -~ 1
    & attachment .~ Unattached


defaultThrowHandler :: V2 -> Handler ()
defaultThrowHandler dir = do
  (cloneLens -> ctxSelf) <- getSelfRef
  ctxSelf %= setBoosting dir False throwStrength throwTime


defaultGrabHandler :: Handler Bool
defaultGrabHandler = do
  (cloneLens -> ctxSelf) <- getSelfRef
  l <- gets $ view ctxLevel
  p <- gets $ view ctxSelf

  let as = l ^. actors ^.. traverse
      isGrabbable a = and
                    [ aGeom a /= aGeom p || _aPos a /= _aPos p
                    , actorsIntersect a p
                    , _grabType a /= Ungrabbable
                    ]

  case listToMaybe $ filter isGrabbable as of
    Just a -> do
      let lo = a ^. self
      case _grabType a of
        Ungrabbable ->
          error "impossible"

        Carry -> do
          ctxSelf . grabData .= Carrying lo
          ctxLevel . cloneLens lo . _Just . jumpData . jumpState .= BeingHeld

        DoAction -> do
          runLocal Nothing
                   lo
                   (a ^. handlers . actionGrabHandler)

      pure True
    Nothing -> pure False


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
doRecovety dt = jumpData . recoveryTime -~ dt


addRecovery :: Actor -> Actor
addRecovery p = p & jumpData . recoveryTime .~ recoverTime

-- deathHandler :: Level -> Actor -> Maybe Actor
-- deathHandler l p =
--   if (any (flip inRect (_aPos p)) $ deathZones l) || _aHealth p <= 0
--      then Nothing
--                       else Just p


defaultStartJumpHandler :: Handler ()
defaultStartJumpHandler = do
  (cloneLens -> ctxSelf) <- getSelfRef
  p <- gets $ view ctxSelf

  when (p ^. jumpData . jumpsLeft > 0) $ do
    ctxSelf %= doJump


defaultStartBoostHandler :: Handler ()
defaultStartBoostHandler = do
  (cloneLens -> ctxSelf) <- getSelfRef
  p    <- gets $ view ctxSelf
  l    <- gets $ view ctxLevel
  ctrl <- asks _ctxController

  when (canBoost l p) $ do
    ctxSelf %= setBoosting (fromJust $ wantsBoost ctrl)
                              True
                              boostStrength
                              boostTime


runHandler
    :: Time
    -> Controller
    -> (Lens' GameState (Maybe Actor))
    -> GameState
    -> Handler a
    -> Writer (Endo GameState) (a, GameState)
runHandler dt ctrl a gs = flip runStateT gs
                        . flip runReaderT (HContext dt ctrl a)


runLocal :: Maybe Controller -> ALens' Level (Maybe Actor) -> Handler a -> Handler a
runLocal c lo = local $ (ctxSelfRef .~ currentLevel . cloneLens lo)
                      . (ctxController .~ maybe (error "no ctrl in local") id c)


doActorHandlers :: Handler ()
doActorHandlers = do
  (cloneLens -> ctxSelf) <- getSelfRef
  Handlers {..} <- gets . view $ ctxSelf . handlers
  ctrl <- asks $ view ctxController

  dt   <- asks $ view ctxTime
  ctxSelf %= doRecovety dt
  _walkHandler

  p <- gets $ view ctxSelf

  case view grabData p of
    Carrying whom -> do
      ctxLevel . cloneLens whom . _Just . aPos .= _aPos p + V2 0 (-30)

    _ -> pure ()

  let doCollide =
        \case
          Just piece -> _collideHandler piece
          Nothing    -> pure ()

  case p ^. attachment of
    Grasping t dir -> _hookHandler t dir

    _ ->
      case p ^. jumpData . jumpState of
        Stand  -> _standHandler
        Jump y -> _jumpHandler y >>= doCollide
        Boost dir strength time applyPenalty ->
          _boostHandler dir strength time applyPenalty >>= doCollide
        BeingHeld -> pure ()

  numJumps <- gets . view $ ctxSelf . jumpData . jumpsLeft
  when (wantsJump ctrl && numJumps > 0) $ do
    _startJumpHandler

  when (isJust (wantsBoost ctrl) && not (isBoosting p) && not (isHooked p)) $ do
    _startBoostHandler

  when (wantsGrasp ctrl && not (isHooked p)) $ do
    case view grabData p of
      Carrying whom -> do
        ctxSelf . grabData .= NotGrabbing

        gets (view $ ctxState . currentLevel . cloneLens whom) >>= \case
          Nothing -> pure ()
          Just you ->
            runLocal Nothing
                     whom
                     (you ^. handlers . throwHandler $ ctrlDir ctrl)

      NotGrabbing -> do
        l <- gets $ view ctxLevel
        case getNearbyHook l p of
          Just t -> do
            ctxSelf %= doLand
            ctxSelf . attachment .= (Grasping t . normalize . set _y 0 $ _aPos p - hookPos t)

          Nothing -> void _grabHandler

  _updateHandler


defaultWalkHandler :: Handler ()
defaultWalkHandler = do
  (cloneLens -> ctxSelf) <- getSelfRef
  dt   <- asks _ctxTime
  ctrl <- asks _ctxController
  p    <- gets $ view ctxSelf
  l    <- gets $ view ctxLevel

  when (not (isHooked p) && canAct p) $ do
    let dir  = view _x . ctrlDir $ ctrl
        pos' = snd . collision l
                               AxisX
                               (aGeom p)
                               (_aPos p)
                   $ walkSpeed * dt * dir

    ctxSelf . aPos .= pos'


defaultStandHandler :: Handler ()
defaultStandHandler = do
  (cloneLens -> ctxSelf) <- getSelfRef
  gs <- gets $ view ctxState
  p  <- gets $ view ctxSelf

  when (not $ stillStanding gs p) $ do
    ctxSelf %= setFalling


defaultHookHandler :: Hook -> V2 -> Handler ()
defaultHookHandler hook dir = do
  (cloneLens -> ctxSelf) <- getSelfRef
  ctrl <- asks _ctxController
  p    <- gets $ view ctxSelf

  case (wantsJump ctrl, wantsGrasp ctrl) of
    (False, False) -> do
      let dir' = if ctrlDir ctrl /= V2 0 0
                    then ctrlDir ctrl
                    else dir
      ctxSelf . attachment .= Grasping hook dir'
      ctxSelf . aPos .= hookPos hook
                         + dir' ^* targetRadius
                         + onSideways
                             (V2 0 0)
                             ((V2 0 0.5) ^* topY (aGeom p))
                             dir'

    (False, True) ->
      ctxSelf %= setBoosting boostDir False boostStrength boostTime

    (True, _) -> do
      ctxSelf %= doJump
      ctxSelf . jumpData . jumpsLeft .= 0
  where
    onSideways a b dir' = bool a b $ view _x dir' /= 0
    boostDir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir


defaultJumpHandler :: Double -> Handler (Maybe Piece)
defaultJumpHandler y = do
  (cloneLens -> ctxSelf) <- getSelfRef
  dt   <- asks _ctxTime
  ctrl <- asks _ctxController
  p    <- gets $ view ctxSelf
  l    <- gets $ view ctxLevel

  let gravity' =
        if ctrlJump ctrl && y < 0
           then gravity * jumpAttenuation
           else gravity
      (collided, pos') = collision l
                                   AxisY
                                   (aGeom p)
                                   (_aPos p)
                                   (dt * y)

  ctxSelf . aPos .= pos'

  case collided of
    Just line -> do
      ctxSelf              %= doLand
      ctxSelf . attachment .= StandingOn line

    Nothing -> do
      ctxSelf . jumpData . jumpState .= Jump (y + (min gravity' terminalVelocity) * dt)

  pure collided



defaultBoostHandler :: V2 -> Double -> Time -> Bool -> Handler (Maybe Piece)
defaultBoostHandler _ _ t _ | t <= 0 = do
  (cloneLens -> ctxSelf) <- getSelfRef
  ctxSelf %= setFalling
  ctxSelf %= addRecovery
  pure Nothing

defaultBoostHandler dir strength t applyPenalty = do
  (cloneLens -> ctxSelf) <- getSelfRef
  dt   <- asks _ctxTime
  p    <- gets $ view ctxSelf
  l    <- gets $ view ctxLevel

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

  ctxSelf . jumpData
             . jumpState .= Boost dir
                                  strength
                                  (t - dt)
                                  applyPenalty
  ctxSelf . aPos .= xy'

  pure wall

