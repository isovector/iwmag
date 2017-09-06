{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Bomb () where

import Actor
import Control.Lens hiding (Level)
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
  (cloneLens -> ctxSelf) <- getSelfRef

  when (g /= "") $ do
    currentLevel . destructableGeometry . at g .= Nothing
    geometryChanged .= True
    ctxSelf . toRemove .= True

