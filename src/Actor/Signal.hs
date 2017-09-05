{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections     #-}

module Actor.Signal where

import           Actor.Constants
import           Actor.JumpState
import           Collision
import           Control.Monad.State (State, get, gets, runState, put)
import           Control.Monad.Trans.Reader (runReaderT)
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
defaultThrowHandler dir =
  hctxPlayer %= setBoosting dir False throwStrength throwTime


defaultGrabHandler :: Handler Bool
defaultGrabHandler = do
  l <- gets $ view hctxLevel
  p <- gets $ view hctxPlayer

  let as = l ^. actors ^.. traverse
      isGrabbable a = and
                    [ aGeom a /= aGeom p
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
          hctxPlayer . grabData .= Carrying lo
          hctxLevel . cloneLens lo . _Just . jumpData . jumpState .= BeingHeld

        DoAction -> do
          runLocal (error "no dt for you")
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


runHandler :: Time -> Controller -> Actor -> Handler a -> State GameState (Actor, a)
runHandler dt ctrl a = swizzle a
                     . flip runReaderT (HContext dt ctrl)
  where
    swizzle :: b -> State (c, b) d -> State c (b, d)
    swizzle b s = do
      c <- get
      let (d, (c', b')) = runState s (c, b)
      put c'
      pure (b', d)


runLocal :: Time -> ALens' Level (Maybe Actor) -> Handler () -> Handler ()
runLocal dt lo h = do
  gets (view $ hctxLevel . cloneLens lo) >>= \case
    Nothing   -> pure ()

    Just whom -> do
      gs   <- gets $ view hctxState
      let ((a', ()), gs') = flip runState gs
                          $ runHandler dt
                                       (error "no ctrl for local")
                                       whom
                                       h
      hctxState .= gs'
      hctxLevel . cloneLens lo ?= a'


runHandlers :: Time -> Controller -> Actor -> State GameState Actor
runHandlers dt ctrl a = fmap fst
                      . runHandler dt ctrl a
                      $ do
  hctxPlayer %= doRecovety dt
  _walkHandler

  p <- gets $ view hctxPlayer

  case view grabData p of
    Carrying whom -> do
      hctxLevel . cloneLens whom . _Just . aPos .= _aPos p + V2 0 (-30)

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

  numJumps <- gets . view $ hctxPlayer . jumpData . jumpsLeft
  when (wantsJump ctrl && numJumps > 0) $ do
    _startJumpHandler

  when (isJust (wantsBoost ctrl) && not (isBoosting p) && not (isHooked p)) $ do
    _startBoostHandler

  when (wantsGrasp ctrl && not (isHooked p)) $ do
    case view grabData p of
      Carrying whom -> do
        hctxPlayer . grabData .= NotGrabbing

        gets (view $ hctxState . currentLevel . cloneLens whom) >>= \case
          Nothing -> pure ()
          Just you ->
            runLocal dt
                     whom
                     (you ^. handlers . throwHandler $ ctrlDir ctrl)

      NotGrabbing -> do
        l <- gets $ view hctxLevel
        case getNearbyHook l p of
          Just t -> do
            hctxPlayer %= doLand
            hctxPlayer . attachment .= (Grasping t . normalize . set _y 0 $ _aPos p - hookPos t)

          Nothing -> void _grabHandler

  _updateHandler

  where
    Handlers {..} = _handlers a


defaultWalkHandler :: Handler ()
defaultWalkHandler = do
  dt   <- asks hctxTime
  ctrl <- asks hctxController
  p    <- gets $ view hctxPlayer
  l    <- gets $ view hctxLevel

  when (not (isHooked p) && canAct p) $ do
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


defaultHookHandler :: Hook -> V2 -> Handler ()
defaultHookHandler hook dir = do
  ctrl <- asks hctxController
  p    <- gets $ view hctxPlayer

  case (wantsJump ctrl, wantsGrasp ctrl) of
    (False, False) -> do
      let dir' = if ctrlDir ctrl /= V2 0 0
                    then ctrlDir ctrl
                    else dir
      hctxPlayer . attachment .= Grasping hook dir'
      hctxPlayer . aPos .= hookPos hook
                         + dir' ^* targetRadius
                         + onSideways
                             (V2 0 0)
                             ((V2 0 0.5) ^* topY (aGeom p))
                             dir'

    (False, True) ->
      hctxPlayer %= setBoosting boostDir False boostStrength boostTime

    (True, _) -> do
      hctxPlayer %= doJump
      hctxPlayer . jumpData . jumpsLeft .= 0
  where
    onSideways a b dir' = bool a b $ view _x dir' /= 0
    boostDir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir


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

