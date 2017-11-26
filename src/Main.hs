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
  void $ newEntity ( Pos $ V2 200 100
                   , Gfx . group
                         $ drawPlayer (rgb 1 1 1) playerGeom
                   , Vel $ V2 30 0
                   , Gravity
                   , Collision playerGeom
                   , Player
                   )

  void $ newEntity ( Pos $ V2 400 100
                   , Gfx . group
                         $ drawActor (rgb 1 1 1) playerGeom
                   , Vel $ V2 (-30) 30
                   )
  pure ()


draw :: Sys Element
draw = do
  ppos <- fmap head . cmapM $ \(Player, Pos p) -> pure p
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


arrows :: Sys V2
arrows = liftIO $ do
  keys <- fmap toEnum <$> getKeyState
  let isDown = flip elem keys
      l = isDown LeftKey
      r = isDown RightKey
      u = isDown UpKey
      d = isDown DownKey

  pure $ uncurry V2
        ( fromIntegral $ -1 * fromEnum l + 1 * fromEnum r
        , fromIntegral $ -1 * fromEnum u + 1 * fromEnum d
        )


step :: Time -> Sys ()
step dt = do
  arrs <- (^* walkSpeed) <$> arrows
  rmap $ \(Player, Vel (V2 _ y)) ->
    Vel $ arrs & _y .~ y


  mwmap all fallHandler

  cimapM_ $ \(e, (Gravity, Vel v)) -> do
    set (cast e) $
      Vel $ v + V2 0 1 ^* (gravity * dt)

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

