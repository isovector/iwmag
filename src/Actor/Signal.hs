{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Actor.Signal where

import Actor.Constants
import Collision
import Game.Sequoia.Keyboard (Key (..))
import Input (input, getKeys)
import Prologue


sweep'
    :: BoxGeom
    -> V2
    -> [Piece]
    -> Axis
    -> Double
    -> (Maybe Piece, V2)
sweep' b v = sweep b v pieceLine


fallHandler
    :: [Piece] -> ECSF
fallHandler ps = do
  with gravity
  p <- get pos
  v <- get vel
  c <- get collision
  StandingOn _ <- get standContext

  let (mp, _) = sweep' c p ps AxisY 1

  pure $ case mp of
    Just l -> defEntity'
      { standContext = Set $ StandingOn l
      }
    Nothing -> defEntity'
      { standContext = Unset
      , vel = Set $ v & _y .~ 0
      }


moveHandler
    :: Time
    -> [Piece]
    -> ECSF
moveHandler dt ps = do
  without heldBy
  p <- get pos
  v <- get evel
  c <- get collision

  let x         = view _x v
      (hit, p') = sweep' c p ps AxisX $ x * dt
  pure $ defEntity'
    { pos = Set p'
    , vel = case hit of
              Just _ -> Set $ v & _x .~ 0
              Nothing -> Keep
    }


dropHandler
    :: Time
    -> [Piece]
    -> ECSF
dropHandler dt ps = do
  without heldBy
  without standContext
  p <- get pos
  v <- get vel
  c <- get collision

  let y         = view _y v
      (hit, p') = sweep' c p ps AxisY $ y * dt
  pure $ defEntity'
    { pos = Set p'
    , vel = maybe Keep (const . Set $ v & _y .~ 0) hit
    , standContext = maybe Unset
                           (\l -> bool Unset
                                       (Set $ StandingOn l)
                                     $ y >= 0)
                           hit
    }


jumpHandler :: Sys ()
jumpHandler = do
  -- set wantsjump for the player
  wj <- _riWantsJump . input <$> getKeys
  emap $ do
    with player
    pure $ defEntity'
      { wantsJump = bool Unset (Set ()) wj
      }

  emap $ do
    v <- get vel
    j <- get jump
    with wantsJump
    guard $ _jCurJumps j /= 0
    guard $ _jJumping  j /= True

    pure $ defEntity'
      { vel = Set $ v & _y .~ -jumpStrength
      , jump = Set $ j & jCurJumps -~ 1
                       & jJumping .~ True
      }

  -- clear standing context if jumping
  emap $ do
    j <- get jump
    guard $ _jJumping j
    pure $ defEntity'
      { standContext = Unset
      }


  -- cancel jJumping if WantsJump isn't set
  emap $ do
    without wantsJump
    j <- get jump
    pure $ defEntity'
      { jump = Set $ j & jJumping .~ False
      }

  -- reset jumpdata if on ground
  emap $ do
    j <- get jump
    StandingOn _ <- get standContext
    pure $ defEntity'
      { jump = Set $ j & jJumping .~ False
                       & jCurJumps .~ view jMaxJumps j
      }


boostHandler :: Time -> Sys ()
boostHandler dt = do
  emap $ do
    b <- get boosting
    pure $ defEntity'
      { vel = Set $ b ^. bBoostVel
      , boosting =
          bool Unset
               (Set $ b & bBoostTime -~ dt)
             $ b ^. bBoostTime > 0

      }

  emap $ do
    without boosting
    cb  <- get canBoost
    dir <- get wantsBoost
    guard $ _cbCurBoosts cb /= 0

    pure $ defEntity'
      { canBoost   = Set $ cb & cbCurBoosts -~ 1
      , wantsBoost = Unset
      , boosting = Set $ Boosting (dir ^* boostStrength) boostTime
      }

  -- clear standing context if boosting
  emap $ do
    with boosting
    pure $ defEntity'
      { standContext = Unset
      }

  -- reset boostdata if on ground
  emap $ do
    StandingOn _ <- get standContext
    b <- get canBoost
    pure $ defEntity'
      { canBoost = Set $ b & cbCurBoosts .~ view cbMaxBoosts b
      }


gravityHandler :: Time -> Sys ()
gravityHandler dt = do
  let gravity' v f = v + V2 0 1 ^* (gravityStrength * dt * f)

  emap $ do
    without heldBy
    without wantsJump
    with gravity
    v <- get vel
    pure $ defEntity' { vel = Set $ gravity' v 1 }

  emap $ do
    without heldBy
    with gravity
    with wantsJump
    v <- get vel
    pure $ defEntity'
      { vel = Set
            . gravity' v
            . bool 1 jumpAttenuation
            $ view _y v < 0
      }


playerHandler :: Time -> Sys ()
playerHandler dt = do
  keys <- getKeys
  let arrsOf = _riArrows . input
      idling = (== V2 0 0)

  emap $ do
    without boosting
    pl <- get player
    v  <- maybe (V2 0 0) id <$> getMaybe xvel

    let oldKeys  = pl ^. pLastInput
        pressed x = elem x keys
                 && elem x oldKeys
        arrs     = arrsOf keys
        oldArrs  = arrsOf oldKeys
        isIdle   = idling arrs
        wasIdle  = idling oldArrs
        timeIdle = pl ^. pIdleTime
        lastDir  = pl ^. pLastDir
        newVel   = v & _x .~ view _x arrs * walkSpeed

        shouldBoost =
          and [ not isIdle
              , wasIdle
              , arrs == lastDir
              , timeIdle <= doubleTapTime
              ]

    pure $ defEntity'
      { xvel = bool Unset (Set newVel) $ not isIdle
      , player = Set
               . Player keys
                        (bool lastDir
                              oldArrs
                              $ isIdle && not wasIdle)
               $ bool 0 (timeIdle + dt) isIdle
      , wantsGrasp =
          bool Unset (Set ()) $ pressed LeftControlKey
      , wantsBoost =
          bool Unset
              (Set $ normalize arrs
                   & _y %~ \y ->
                        y * bool 1
                                 boostUpPenalty
                                 (y < 0)
              )
              shouldBoost
      }


swoopHandler :: Time -> BoxGeom -> V2 -> ECSF
swoopHandler dt (topY -> h) v2 = do
  p  <- get pos
  sw <- get swoop

  case _swPhase sw of
    SwoopHover -> do
      let p' = v2 + _swOffset sw
          dp = p' - p
          speed = 400
          target = 20
          a' = normalize dp ^* speed
          hoverOver = _swHoverTime sw <= 0

      pure $ defEntity'
        { acc = bool Keep
                    (Set a')
                    $ norm dp >= target
        , swoop = Set $ sw
                      & swHoverTime -~ dt
                      & swPhase .~ bool SwoopHover
                                        SwoopSwing
                                        hoverOver
        , termVel = Set 250
        , dangerous = no
        , hitbox = Unset
        }

    SwoopSwing -> do
      let p' = v2 + V2 0 (negate $ h / 2)
          dp = p' - p
          speed = 2000
          a' = normalize dp ^* speed

      pure $ defEntity'
        { acc = Set a'
        , termVel = Set 400
        , dangerous = yes
        , hitbox = Set
                 . Hitbox (Just 24)
                 $ ActionParryable
                   ( ActionImpartVelocity a'
                  <> ActionImpartDamage 18
                   )
                <> ActionCallback
                 ( do
                     sw' <- get swoop
                     pure defEntity'
                       { swoop = Set $ sw'
                               & swPhase     .~ SwoopHover
                               & swHoverTime .~ _swMaxHoverTime sw
                       }

                 )
        }


hitboxHandler :: Sys ()
hitboxHandler = do
  boxes  <- efor $ \ent ->
    (ent,,,) <$> get pos <*> get hitbox <*> getMaybe collision
  hittees <- efor $ \ent ->  do
    with hitboxable
    (ent,,) <$> get pos <*> get collision

  for_ boxes $ \(boxent, boxpos, box, boxgeom) ->
    for_ hittees $ \(ent, pos, geom) ->
      case (_hbRadius box, boxgeom) of
        (Just radius, _) ->
          when (withinRadius geom pos radius boxpos) $
            actionHandler boxent ent (_hbAction box)
        (Nothing, Just bgeom) ->
          when (boxesIntersect bgeom boxpos geom pos) $
            actionHandler boxent ent (_hbAction box)
        (Nothing, Nothing) ->
          error "attempted to do a hitbox handler with no collision or radius"



parryHandler :: Time -> Sys ()
parryHandler dt = do
  parried <- fmap (not . null) . efor . const $ do
    with player
    get wantsGrasp

  when parried $ do
    owners parryTimer >>= traverse_ (destroy . fst)

  emap $ do
    pt <- get parryTimer
    pure $ defEntity'
      { parryTimer = Set $ pt & ptTime -~ dt
      }

  elapsed <- efor $ \ent -> do
    pt <- get parryTimer
    guard $ pt ^. ptTime <= 0
    pure (ent, pt)

  for_ elapsed $ \(ent, ParryTimer{..}) -> do
    actionHandler _ptBoxEnt _ptEnt _ptAction
    destroy ent


actionHandler :: Ent -> Ent -> Action -> Sys ()
actionHandler _ _ ActionDoNothing = pure ()
actionHandler box ent (ActionCombine a b) =
  actionHandler box ent a >> actionHandler box ent b
actionHandler _ ent ActionResetJumps =
  runAndSet ent $ do
    j <- get jump
    pure $ defEntity'
      { jump = Set $ j
                   & jCurJumps .~ (view jMaxJumps j - 1)
      }
actionHandler _ ent (ActionImpartDamage dmg) =
  runAndSet ent $ do
    hp <- get hitpoints
    pure $ defEntity'
      { hitpoints = Set $ hp
                        & hpCurrent %~ max 0 . subtract dmg
      }
actionHandler _ ent (ActionImpartVelocity v2) =
  setEntity ent defEntity' { vel = Set v2 }
actionHandler ent _ (ActionCallback cb) = runAndSet ent cb
actionHandler bent ent (ActionParryable action) =
  void $ newEntity defEntity
    { parryTimer = Just $ ParryTimer parryTime bent ent action
    }


heldByHandler :: Sys ()
heldByHandler = do
  let getCol = fromMaybe (BoxGeom 0 0 0 0) <$> getMaybe collision

  holding <- getEntsWith heldBy
  for_ holding $ \(eholdee, eholder) -> do
    Just p <- runQueryT eholder $ do
      p <- get pos
      c <- getCol
      pure $ p - V2 0 (topY c)
    Just p' <- runQueryT eholdee $ do
      c <- getCol
      pure . negate . V2 0 $ bottomY c

    setEntity eholdee defEntity'
      { pos = Set $ p + p' }


throwHandler :: Sys ()
throwHandler = do
  throwing <- sortBy (comparing fst)             <$> getEntsWith wantsThrow
  held     <- sortBy (comparing fst) . fmap swap <$> getEntsWith heldBy
  for_ (fmap fst throwing) $
    flip setEntity defEntity'
      { wantsThrow = Unset }

  for_ (zipAssocWith (,) throwing held) $ \(_, (throwDir, eholdee)) -> do
    setEntity eholdee defEntity'
      { heldBy = Unset
      , vel = Set $ throwDir ^* 100
      }


getEntsWith :: (Entity -> Maybe a) -> Sys [(Ent, a)]
getEntsWith f = efor $ \e -> (e,) <$> get f


runAndSet :: Ent -> ECSF -> Sys ()
runAndSet ent m = do
  setter <- runQueryT ent m
  for_ setter $ setEntity ent

