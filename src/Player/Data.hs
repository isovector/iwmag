module Player.Data ( Player
                   , pPos
                   , jumpState
                   , jumpsLeft
                   , hasBoosted
                   , standingOn
                   , defaultPlayer
                   ) where

import ClassyPrelude
import Math

import Player.Constants
import Player.Controller
import Player.JumpState

data Player =
    Player { pPos       :: Vector2
           , jumpState  :: JumpState
           , jumpsLeft  :: Int
           , hasBoosted :: Bool
           , standingOn :: Maybe Line
           } deriving (Show)

defaultPlayer :: Player
defaultPlayer = Player { pPos       = Vector2 650 100
                       , jumpState  = Stand
                       , jumpsLeft  = jumpCount
                       , hasBoosted = False
                       , standingOn = Nothing
                       }
