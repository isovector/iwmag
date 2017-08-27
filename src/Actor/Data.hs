{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Data
  ( Actor (..)
  , ActorAttachment (..)
  , defaultActor
  ) where

import Actor.Constants
import Types

defaultActor :: Actor
defaultActor = Actor
  { aPos         = V2 100 100
  , jumpState    = Stand
  , jumpsLeft    = jumpCount
  , boostsLeft   = boostCount
  , recoveryTime = 0
  , attachment   = Unattached
  , aGeom        = playerGeom
  }

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 24
  , bottomY = 0
  , leftX   = 8
  , rightX  = 8
  }

