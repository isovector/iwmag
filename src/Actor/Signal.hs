{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module Actor.Signal where

import Actor.Constants
import Collision
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
  p <- get pos
  v <- get vel
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
  wj <- snd . input <$> getKeys
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
    without wantsJump
    with gravity
    v <- get vel
    pure $ defEntity' { vel = Set $ gravity' v 1 }

  emap $ do
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
  let arrsOf = fst . input
      idling = (== V2 0 0)

  emap $ do
    without boosting
    pl <- get player
    v  <- get vel

    let oldKeys  = pl ^. pLastInput
        arrs     = arrsOf keys
        oldArrs  = arrsOf oldKeys
        isIdle   = idling arrs
        wasIdle  = idling oldArrs
        timeIdle = pl ^. pIdleTime
        lastDir  = pl ^. pLastDir
        -- TODO(sandy): do something clever here so stopping walking
        -- doesnt have slide
        newVel   = v & _x .~ view _x arrs * walkSpeed

        shouldBoost =
          and [ not isIdle
              , wasIdle
              , arrs == lastDir
              , timeIdle <= doubleTapTime
              ]

    pure $ defEntity' -- Safe @(Vel, Player, WantsBoost)
      { vel = bool Keep (Set newVel) $ not isIdle
      , player = Set
               . Player keys
                        (bool lastDir
                              oldArrs
                              $ isIdle && not wasIdle)
               $ bool 0 (timeIdle + dt) isIdle
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
                 . Hitbox 24
                 $ ActionImpartVelocity a'
                <> ActionImpartDamage 18
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
    (ent,,) <$> get pos <*> get hitbox
  hittees <- efor $ \ent ->  do
    with hitboxable
    (ent,,) <$> get pos <*> get collision

  for_ boxes $ \(boxent, boxpos, box) ->
    for_ hittees $ \(ent, pos, geom) ->
      when (withinRadius geom pos (_hbRadius box) boxpos) $
        actionHandler boxent ent (_hbAction box)


actionHandler :: Ent -> Ent -> Action -> Sys ()
actionHandler _ _ ActionDoNothing = pure ()
actionHandler box ent (ActionCombine a b) =
  actionHandler box ent a >> actionHandler box ent b
actionHandler _ ent (ActionImpartDamage dmg) =  do
  setter <- runQueryT ent $ do
    hp <- get hitpoints
    pure $ defEntity'
      { hitpoints = Set $ hp
                        & hpCurrent %~ max 0 . subtract dmg
      }
  for_ setter $ setEntity ent
actionHandler _ ent (ActionImpartVelocity v2) =
  setEntity ent defEntity' { vel = Set v2 }
actionHandler ent _ (ActionCallback cb) = do
  setter <- runQueryT ent cb
  for_ setter $ setEntity ent

