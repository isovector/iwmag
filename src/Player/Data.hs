{-# LANGUAGE NoImplicitPrelude #-}

module Player.Data ( Player
                   , pPos
                   , jumpState
                   , jumpsLeft
                   , boostsLeft
                   , standingOn
                   , defaultPlayer
                   ) where

import BasePrelude
import Game.Sequoia
import Math
import Player.Constants
import Player.JumpState


data Player = Player
  { pPos       :: !V2
  , jumpState  :: !JumpState
  , jumpsLeft  :: !Int
  , boostsLeft :: !Int
  , standingOn :: Maybe Line
  } deriving (Show)


defaultPlayer :: Player
defaultPlayer = Player
  { pPos       = V2 100 100
  , jumpState  = Stand
  , jumpsLeft  = jumpCount
  , boostsLeft = boostCount
  , standingOn = Nothing
  }

