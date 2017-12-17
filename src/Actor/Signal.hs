{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Actor.Signal where

import           Prologue
import           Collision

sweep' :: BoxGeom -> V2 -> [Piece] -> Axis -> Double -> (Maybe Piece, V2)
sweep' b v = sweep b v pieceLine


-- getNearbyHook :: Level -> Actor -> Maybe Hook
-- getNearbyHook l p = getFirst
--                   . mconcat
--                   . fmap intersectsWithActor
--                   $ targets l
--   where
--     intersectsWithActor t
--       = First
--       . bool Nothing (Just t)
--       . boxesIntersect (aGeom p)
--                        (_aPos p)
--                        (BoxGeom targetRadius
--                                 targetRadius
--                                 targetRadius
--                                 targetRadius)
--       $ hookPos t



collision
    :: World EntWorld
    => Axis
    -> BoxGeom
    -> V2
    -> Double
    -> Sys (Maybe Piece, V2)
collision ax geom pos dx = do
  pes <- owners geometry
  let ps = mapMaybe (geometry . snd) pes

  pure $ sweep' geom pos ps ax dx


-- defaultThrowHandler :: V2 -> Handler ()
-- defaultThrowHandler dir = do
--   (cloneLens -> ctxSelf) <- getSelfRef
--   ctxSelf %= setBoosting dir False throwStrength throwTime


-- defaultGrabHandler :: Handler Bool
-- defaultGrabHandler = do
  -- (cloneLens -> ctxSelf) <- getSelfRef
  -- (cloneLens -> ctxSelf) <- getSelfRef
  -- l <- use ctxLevel
  -- p <- use ctxSelf

  -- let as = l ^. actors ^.. traverse
  --     isGrabbable a = and
  --                   [ aGeom a /= aGeom p || _aPos a /= _aPos p
  --                   , actorsIntersect a p
  --                   , _grabType a /= Ungrabbable
  --                   ]

  -- case listToMaybe $ filter isGrabbable as of
  --   Just a -> do
  --     let lo = a ^. self
  --     case _grabType a of
  --       Ungrabbable ->
  --         error "impossible"

  --       Carry -> do
  --         ctxSelf . grabData .= Carrying lo
  --         ctxLevel . cloneLens lo . _Just . jumpData . jumpState .= BeingHeld

  --       DoAction -> do
  --         runLocal Nothing
  --                  lo
  --                  (a ^. handlers . actionGrabHandler)

  --     pure True
  --   Nothing -> pure False



-- deathHandler :: Level -> Actor -> Maybe Actor
-- deathHandler l p =
--   if (any (flip inRect (_aPos p)) $ deathZones l) || _aHealth p <= 0
--      then Nothing
--                       else Just p




-- doActorHandlers :: Handler ()
-- doActorHandlers = do
  -- (cloneLens -> ctxSelf) <- getSelfRef
  -- Handlers {..} <- use $ ctxSelf . handlers
  -- ctrl <- view ctxController

  -- case view grabData p of
  --   Carrying whom -> do
  --     ctxLevel . cloneLens whom . _Just . aPos .= _aPos p + V2 0 (-30)

  --   _ -> pure ()

  -- let doCollide =
  --       \case
  --         Just piece -> _collideHandler piece
  --         Nothing    -> pure ()

  -- case p ^. attachment of
  --   Grasping t dir -> _hookHandler t dir

  --   _ ->
  --     case p ^. jumpData . jumpState of
  --       Stand  -> _standHandler
  --       Jump y -> _jumpHandler y >>= doCollide
  --       Boost dir strength time applyPenalty ->
  --         _boostHandler dir strength time applyPenalty >>= doCollide
  --       BeingHeld -> pure ()

  -- numJumps <- use $ ctxSelf . jumpData . jumpsLeft
  -- when (wantsJump ctrl && numJumps > 0) $ do
  --   _startJumpHandler

  -- when (wantsGrasp ctrl && not (isHooked p)) $ do
  --   case view grabData p of
  --     Carrying whom -> do
  --       ctxSelf . grabData .= NotGrabbing

  --       use (currentLevel . cloneLens whom) >>= \case
  --         Nothing -> pure ()
  --         Just you ->
  --           runLocal Nothing
  --                    whom
  --                    (you ^. handlers . throwHandler $ ctrlDir ctrl)

  --     NotGrabbing -> do
  --       l <- use ctxLevel
  --       case getNearbyHook l p of
  --         Just t -> do
  --           ctxSelf %= doLand
  --           ctxSelf . attachment .= (Grasping t . normalize . set _y 0 $ _aPos p - hookPos t)

  --         Nothing -> void _grabHandler

  -- _updateHandler


-- defaultHookHandler :: Hook -> V2 -> Handler ()
-- defaultHookHandler hook dir = do
  -- (cloneLens -> ctxSelf) <- getSelfRef
  -- ctrl <- view ctxController
  -- p    <- use ctxSelf

  -- case (wantsJump ctrl, wantsGrasp ctrl) of
  --   (False, False) -> do
  --     let dir' = if ctrlDir ctrl /= V2 0 0
  --                   then ctrlDir ctrl
  --                   else dir
  --     ctxSelf . attachment .= Grasping hook dir'
  --     ctxSelf . aPos .= hookPos hook
  --                        + dir' ^* targetRadius
  --                        + onSideways
  --                            (V2 0 0)
  --                            ((V2 0 0.5) ^* topY (aGeom p))
  --                            dir'

  --   (False, True) ->
  --     ctxSelf %= setBoosting boostDir False boostStrength boostTime

  --   (True, _) -> do
  --     ctxSelf %= doJump
  --     ctxSelf . jumpData . jumpsLeft .= 0
  -- where
  --   onSideways a b dir' = bool a b $ view _x dir' /= 0
  --   boostDir = normalize $ dir + onSideways (V2 0 0) (V2 0 $ -1) dir

