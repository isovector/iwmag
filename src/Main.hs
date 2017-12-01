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
import           Actor.Signal (collision)
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
  void $ newEntity
       ( Pos $ V2 200 100
       , Gfx . group
             $ drawPlayer (rgb 1 1 1) playerGeom
       , Vel $ V2 0 0
       , Gravity
       , Collision playerGeom
       , Jump     jumpCount  0 True
       , CanBoost boostCount 0
       , Player [] (V2 0 0) 10
       )


draw :: Sys Element
draw = do
  ppos <- fmap head . cmapM $ \(Player{}, Pos p) -> pure p
  CurLevel l <- getGlobal

  let cam    = clampCamera (levelSize l) ppos
      center = V2 (fromIntegral gameWidth) (fromIntegral gameHeight) ^* 0.5

  geom <- cmapM $ \(Geometry _, Gfx g) -> pure g
  gfx  <- cmapM $ \(Pos p, Gfx g) -> pure $ move p g
  pure . collage gameWidth gameHeight
       . pure
       . move (center - cam)
       . group
       $ geom ++ gfx


fallHandler
    :: (Gravity, Pos, Vel, Collision, StandContext)
    -> Sys (Safe (StandContext, Vel))
fallHandler (Gravity, Pos p, Vel v, Collision c, StandingOn _) = do
  (mp, _) <- collision AxisY c p 1

  pure . Safe $ case mp of
    Just l ->
      ( Just $ StandingOn l
      , Just $ Vel v
      )
    Nothing ->
      ( Nothing
      , Just . Vel $ v & _y .~ 0
      )


moveHandler
    :: Time
    -> (Pos, Vel, Collision)
    -> System World (Pos, Vel)
moveHandler dt (Pos p, Vel v@(V2 x _), Collision c) = do
  (hit, p') <- collision AxisX c p $ x * dt
  pure . (Pos p', ) $ case hit of
    Just _  -> Vel $ v & _x .~ 0
    Nothing -> Vel v


dropHandler
    :: Time
    -> (Pos, Vel, Collision)
    -> System World (Safe (Pos, Vel, StandContext))
dropHandler dt (Pos p, Vel v@(V2 _ y), Collision c) = do
  (hit, p') <- collision AxisY c p $ y * dt
  pure . Safe $ case hit of
    Just l  ->
      ( Just $ Pos p'
      , Just . Vel $ v & _y .~ 0
      , if y >= 0
          then Just $ StandingOn l
          else Nothing
      )
    Nothing ->
      ( Just $ Pos p'
      , Just $ Vel v
      , Nothing
      )


jumpHandler :: Sys ()
jumpHandler = do
  -- set wantsjump for the player
  wj <- snd . input <$> getKeys
  rmap' $ \Player{} -> Safe @WantsJump wj

  rmap doJumpHandler

  -- clear standing context if jumping
  rmap' $
    let f (_jJumping -> True, _) = Safe @StandContext Nothing
        f (_, c)                 = Safe (Just c)
     in f

  -- cancel jJumping if WantsJump isn't set
  mmap (without @WantsJump) $ \j ->
    pure $ j & jJumping .~ False

  -- reset jumpdata if on ground
  rmap $ \(StandingOn _, j@Jump{}) ->
    j & jJumping .~ False
      & jCurJumps .~ view jMaxJumps j

 where
  doJumpHandler (v, j@(_jCurJumps -> 0), _)    = (v, j)
  doJumpHandler (v, j@(_jJumping  -> True), _) = (v, j)
  doJumpHandler (Vel v, j, WantsJump) =
    ( Vel $ v & _y  .~ -jumpStrength
    , j & jCurJumps -~ 1
        & jJumping  .~ True
    )


boostHandler :: Time -> Sys ()
boostHandler dt = do
  rmap' $ \b@Boosting{} ->
    Safe @(Vel, Boosting)
         ( Just . Vel $ b ^. bBoostVel
         , bool Nothing
                (Just $ b & bBoostTime -~ dt)
                $ b ^. bBoostTime > 0
         )

  mwmap (without @Boosting) $ pure . doBoostHandler

  -- clear standing context if boosting
  rmap' $ \(Boosting{}) ->
    Safe @StandContext Nothing

  -- reset boostdata if on ground
  rmap $ \(StandingOn _, b@CanBoost{}) ->
    b & cbCurBoosts .~ view cbMaxBoosts b

 where
  doBoostHandler (cb@(_cbCurBoosts -> 0), _) =
    Safe (Just cb, Nothing, Nothing)
  doBoostHandler (cb, WantsBoost dir) =
    Safe @(CanBoost, WantsBoost, Boosting)
         ( Just $ cb & cbCurBoosts -~ 1
         , Nothing
         , Just $ Boosting (dir ^* boostStrength) boostTime
         )


gravityHandler :: Time -> Sys ()
gravityHandler dt = do
  let gravity' v f = Vel $ v + V2 0 1 ^* (gravity * dt * f)

  mmap (without @WantsJump) $ \(Gravity, Vel v) ->
    pure $ gravity' v 1

  rmap $ \(Gravity, Vel v, WantsJump) ->
    gravity' v . bool 1 jumpAttenuation
               $ view _y v < 0


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

  mwmap (without @Boosting) $
    \(pl@Player{}, Vel v) -> do
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

      pure $ Safe @(Vel, Player, WantsBoost)
           ( Just . Vel
                  . bool v newVel
                  $ not isIdle
           , Just . Player keys
                           (bool lastDir
                                 oldArrs
                                 $ isIdle && not wasIdle)
                  $ bool 0 (timeIdle + dt) isIdle
           , bool Nothing
                  (Just . WantsBoost
                        $ normalize arrs
                        & _y %~ \y ->
                            y * bool 1
                                     boostUpPenalty
                                     (y < 0)
                  )
                  shouldBoost
           )


step :: Time -> Sys ()
step dt = do
  -- arrs <- fst <$> input
  -- rmap' $ \Player{} -> Safe @WantsBoost
  --                  . bool Nothing
  --                         (Just $ WantsBoost $ normalize arrs)
  --                  $ arrs /= V2 0 0

  rmap $ \(StandingOn l, Vel v) ->
    Vel $ v ^* (1 - dt * pieceFriction l)


  playerHandler dt

  mwmap all fallHandler
  gravityHandler dt
  boostHandler dt
  jumpHandler

  -- no collision, so do stupid velocity transfer
  mmap (without @Collision) $ \(Pos p, Vel v) ->
    pure . Pos $ p + v ^* dt

  mmap all $ moveHandler dt
  mwmap (without @StandContext) $ dropHandler dt

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



-- runGame :: Engine -> N (B Element)
-- runGame _ = do
--   rController <- getKeyboard
--   rOldCtrller <- sample $ delayTime (deltaTime clock) [] rController

--   (gameAndCtrl, _) <- foldmp (initState, initController) $ \(g, ctrl) -> do
--     controllers <- traverse (sample . aController)
--                  $ g ^. currentLevel . actors

--     let ctrl' = foldController dt rctrl ctrl
--     pure $ (update dt ctrl' controllers g, ctrl')

--   return $ do
--     (game', _) <- sample gameAndCtrl
--     pure $ render game'

main :: IO ()
main = do
  w      <- initWorld
  engine <- startup config
  start  <- realToFrac <$> getPOSIXTime

  flip runSystem w $ do
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

