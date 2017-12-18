{-# LANGUAGE AllowAmbiguousTypes              #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# LANGUAGE TemplateHaskell                  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

-- import           Actor
-- import           Actor.Controller
-- import           Game.Sequoia.Keyboard
import           Actor
import           Actor.Constants
import           Actor.Signal (sweep')
import           Collision (Axis (..))
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Game.Sequoia (startup, render, EngineConfig (..))
import           Game.Sequoia.Keyboard hiding (isDown, arrows)
import           KnotTying (theLevels)
import           Level.Level
import           Math (clamp')
import           Prologue hiding (last)
import qualified SDL.Raw as SDL


getNow :: MonadIO m => m Double
getNow = liftIO $ realToFrac <$> getPOSIXTime


initialize :: Sys ()
initialize = do
  loadLevel . fromJust $ lookup "test1" theLevels
  void $ newEntity $ defEntity
       { pos = Just $ V2 200 100
       , gfx = Just
             . group
             $ drawPlayer (rgb 1 1 1) playerGeom
       , vel       = Just $ V2 0 0
       , gravity   = Just ()
       , collision = Just $ playerGeom
       , jump      = Just $ Jump jumpCount 0 True
       , canBoost  = Just $ CanBoost boostCount 0
       , player    = Just $ Player [] (V2 0 0) 10
       }


draw :: Sys Element
draw = do
  ppos <- fmap head . efor . const $ do
    with player
    get pos
  l <- getGlobal _currentLevel

  let cam    = clampCamera (levelSize l) ppos
      center = V2 (fromIntegral gameWidth) (fromIntegral gameHeight) ^* 0.5

  geom <- efor . const $ do
    with geometry
    get gfx
  gfx  <- efor . const $ do
    p <- get pos
    g <- get gfx
    pure $ move p g
  pure . collage gameWidth gameHeight
       . pure
       . move (center - cam)
       . group
       $ geom ++ gfx


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


-- jumpHandler :: Sys ()
-- jumpHandler = do
--   -- set wantsjump for the player
--   wj <- snd . input <$> getKeys
--   rmap' $ \Player{} -> Safe @WantsJump wj

--   rmap doJumpHandler

--   -- clear standing context if jumping
--   rmap' $
--     let f (_jJumping -> True, _) = Safe @StandContext Nothing
--         f (_, c)                 = Safe (Just c)
--      in f

--   -- cancel jJumping if WantsJump isn't set
--   mmap (without @WantsJump) $ \j ->
--     pure $ j & jJumping .~ False

--   -- reset jumpdata if on ground
--   rmap $ \(StandingOn _, j@Jump{}) ->
--     j & jJumping .~ False
--       & jCurJumps .~ view jMaxJumps j

--  where
--   doJumpHandler (v, j@(_jCurJumps -> 0), _)    = (v, j)
--   doJumpHandler (v, j@(_jJumping  -> True), _) = (v, j)
--   doJumpHandler (Vel v, j, WantsJump) =
--     ( Vel $ v & _y  .~ -jumpStrength
--     , j & jCurJumps -~ 1
--         & jJumping  .~ True
--     )


-- boostHandler :: Time -> Sys ()
-- boostHandler dt = do
--   rmap' $ \b@Boosting{} ->
--     Safe @(Vel, Boosting)
--          ( Just . Vel $ b ^. bBoostVel
--          , bool Nothing
--                 (Just $ b & bBoostTime -~ dt)
--                 $ b ^. bBoostTime > 0
--          )

--   mwmap (without @Boosting) $ pure . doBoostHandler

--   -- clear standing context if boosting
--   rmap' $ \(Boosting{}) ->
--     Safe @StandContext Nothing

--   -- reset boostdata if on ground
--   rmap $ \(StandingOn _, b@CanBoost{}) ->
--     b & cbCurBoosts .~ view cbMaxBoosts b

--  where
--   doBoostHandler (cb@(_cbCurBoosts -> 0), _) =
--     Safe (Just cb, Nothing, Nothing)
--   doBoostHandler (cb, WantsBoost dir) =
--     Safe @(CanBoost, WantsBoost, Boosting)
--          ( Just $ cb & cbCurBoosts -~ 1
--          , Nothing
--          , Just $ Boosting (dir ^* boostStrength) boostTime
--          )


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


getKeys :: Sys [Key]
getKeys = liftIO $ fmap toEnum <$> getKeyState


input :: [Key] -> (V2, Bool)
input keys =
  let isDown = flip elem keys
      l = isDown LeftKey
      r = isDown RightKey
      u = isDown UpKey
      d = isDown DownKey
      j = isDown LeftShiftKey
   in
    ( uncurry V2
      ( fromIntegral $ -1 * fromEnum l + 1 * fromEnum r
      , fromIntegral $ -1 * fromEnum u + 1 * fromEnum d
      )
    , j
    )


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


step :: Time -> Sys ()
step dt = do
  emap $ do
    StandingOn l <- get standContext
    v <- get vel
    pure $ defEntity'
      { vel = Set $ v ^* (1 - dt * pieceFriction l)
      }

  playerHandler dt

  ps <- efor . const $ get geometry
  emap $ fallHandler ps
  gravityHandler dt
  -- boostHandler dt
  -- jumpHandler

  -- no collision, so do stupid velocity transfer
  emap $ do
    without collision
    p <- get pos
    v <- get vel
    pure $ defEntity' { pos = Set $ p + v ^* dt }

  emap $ moveHandler dt ps
  emap $ dropHandler dt ps

  pure ()


clampCamera :: V2  -- ^ Level size.
            -> V2  -- ^ Focal point.
            -> V2
clampCamera (V2 rx ry)
            (V2 x y) = result
  where
    sx = fromIntegral gameWidth
    sy = fromIntegral gameHeight
    w = sx / 2
    h = sy / 2
    result = V2 (clamp' w (rx - w) x) (clamp' h (ry - h) y)


main :: IO ()
main = do
  engine <- startup config
  start  <- realToFrac <$> getPOSIXTime

  flip runSystemT defWorld (Globals $ error "no level loaded") $ do
    initialize
    flip fix start $ \loop last -> do
      now <- getNow
      step $ now - last

      scene <- draw
      liftIO $ render engine scene (gameWidth, gameHeight)

      shouldQuit <- liftIO $ SDL.quitRequested
      unless shouldQuit $ loop now

  where
    config = EngineConfig
               (gameWidth, gameHeight)
               "IWMAG2"
               $ rgb 0.8 0.8 0.8

