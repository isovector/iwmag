{-# LANGUAGE NoImplicitPrelude #-}

module Player.Data
  ( Player (..)
  , PlayerAttachment (..)
  , defaultPlayer
  ) where

import BasePrelude
import Game.Sequoia
import Math
import Player.Constants
import Player.JumpState
import Level.Level

-- TODO(sandy): UMM WHAT NAME
data PlayerAttachment
  = Unattached
  | StandingOn Line
  | Grasping Target V2
  deriving (Eq, Show)


data Player = Player
  { pPos         :: !V2
  , jumpState    :: !JumpState
  , jumpsLeft    :: !Int
  , boostsLeft   :: !Int
  , recoveryTime :: !Time
  , attachment   :: PlayerAttachment
  } deriving (Show)


defaultPlayer :: Player
defaultPlayer = Player
  { pPos         = V2 100 100
  , jumpState    = Stand
  , jumpsLeft    = jumpCount
  , boostsLeft   = boostCount
  , recoveryTime = 0
  , attachment   = Unattached
  }

