module Player.Data ( Player
                   , pPos
                   , jumpState
                   , hasBoosted
                   , standingOn
                   , defaultPlayer
                   ) where

import ClassyPrelude
import Math
import Player.Controller
import Player.JumpState

data Player =
    Player { pPos       :: Vector2
           , jumpState  :: JumpState
           , hasBoosted :: Bool
           , standingOn :: Maybe Line
           } deriving (Show)

defaultPlayer :: Player
defaultPlayer = Player { pPos = Vector2 100 100
                       , jumpState = Stand
                       , hasBoosted = False
                       , standingOn = Nothing
                       }
