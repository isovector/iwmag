{-# LANGUAGE NoImplicitPrelude #-}

module Player.Data
  ( Player (..)
  , PlayerAttachment (..)
  , defaultPlayer
  ) where

import Player.Constants
import Types

defaultPlayer :: Player
defaultPlayer = Player
  { pPos         = V2 100 100
  , jumpState    = Stand
  , jumpsLeft    = jumpCount
  , boostsLeft   = boostCount
  , recoveryTime = 0
  , attachment   = Unattached
  }

