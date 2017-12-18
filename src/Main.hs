{-# LANGUAGE AllowAmbiguousTypes              #-}
{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE NoImplicitPrelude                #-}
{-# LANGUAGE TemplateHaskell                  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           Actor
import           Actor.Constants
import           Actor.Signal
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Game.Sequoia (startup, render, EngineConfig (..))
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
  boostHandler dt
  jumpHandler

  (ppos, pgeom) <- fmap head . efor . const $ do
    with player
    ppos  <- get pos
    pgeom <- get collision
    pure (ppos, pgeom)
  emap $ swoopHandler dt pgeom ppos

  emap $ do
    a <- get acc
    v <- get vel
    pure $ defEntity' { vel = Set $ v + a ^* dt }

  emap $ do
    v    <- get vel
    term <- get termVel
    let v' = normalize v ^* term
    pure $ defEntity' { vel = Set $ bool v' v $ norm v <= term }

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

