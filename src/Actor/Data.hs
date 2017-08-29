{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Data
  ( Actor (..)
  , ActorAttachment (..)
  , defaultActor
  ) where

import Actor.Constants
import Game.Sequoia.Color
import Types

defaultActor :: Actor
defaultActor = Actor
  { _aPos        = V2 100 100
  , _aHealth     = 100
  , jumpState    = Stand
  , jumpsLeft    = jumpCount
  , boostsLeft   = boostCount
  , recoveryTime = 0
  , attachment   = Unattached
  , aGeom        = playerGeom
  , aColor       = white
  , graspTarget  = Unarmed
  }

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 36
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

