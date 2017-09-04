{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Data
  ( Actor (..)
  , ActorAttachment (..)
  , defaultActor
  ) where

import Actor.Constants
import Game.Sequoia.Color
import Actor.Signal
import Types

defaultActor :: Actor
defaultActor = Actor
  { _aPos        = V2 100 100
  , _aHealth     = 100
  , _jumpData = JumpData
    { _jumpState    = Stand
    , _jumpsLeft    = jumpCount
    , _boostsLeft   = boostCount
    , _recoveryTime = 0
    }
  , _attachment  = Unattached
  , aGeom        = playerGeom
  , aColor       = white
  , graspTarget  = Unarmed
  , handlers     =
      Handlers
      { walkHandler       = defaultWalkHandler
      , standHandler      = defaultStandHandler
      , startJumpHandler  = defaultStartJumpHandler
      , jumpHandler       = defaultJumpHandler
      , startBoostHandler = defaultStartBoostHandler
      , boostHandler      = defaultBoostHandler
      , collideHandler    = const $ pure ()
      , hookHandler       = defaultHookHandler
      }

  }

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 36
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

