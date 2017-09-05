{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Bomb () where

import Actor
import Actor.Constants
import Actor.Controller
import Collision (actorsIntersect)
import Control.Lens hiding (Level)
import Control.Monad.State (evalState)
import Game.Sequoia.Color
import Types

bombGeom :: BoxGeom
bombGeom = BoxGeom
  { topY    = 24
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

instance IsObject "bomb" where
  spawn pos _ =
    defaultActor
    { aColor = black
    , aGeom = bombGeom
    , aRender =
        flip drawWithGeom $ \col size ->
          filled col . circle $ view _x size * 0.5
    } & handlers . collideHandler .~ onCollide
      & aPos .~ pos
      & grabType .~ Carry


onCollide :: Piece -> Handler ()
onCollide p = do
  let g = pieceGroup p

  when (g /= "") $ do
    hctxState . currentLevel . destructableGeometry . at g .= Nothing
    hctxState . geometryChanged .= True
    hctxPlayer . toRemove .= True


--   grasp (_fActor -> a) = do
--     p  <- asks ctxPlayer
--     lo <- cloneLens <$> asks ctxLens
--     pure $ case actorsIntersect a p of
--       True  -> Just (Bomb True a, hold lo, id)
--       False -> Nothing
--     where
--       hold lo = Holding
--        { updateHeld = \_ p' ->
--            currentLevel . lo . _Just . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
--        , onThrow = \_ dir l ->
--           l & currentLevel . lo . _Just . internalObj . held   .~ False
--             & currentLevel . lo . _Just . internalObj . fActor %~ setBoosting dir False throwStrength throwTime
--        }

