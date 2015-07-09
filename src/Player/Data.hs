module Player.Data ( Player
                   , pPos
                   , ctrls
                   , jumpState
                   , hasBoosted
                   , defaultPlayer
                   ) where

import Math
import Player.Controller
import Player.JumpState

data Player =
    Player { pPos       :: Vector2
           , ctrls      :: Controller
           , jumpState  :: JumpState
           , hasBoosted :: Bool
           } deriving (Show)

defaultPlayer :: Player
defaultPlayer = Player { pPos = Vector2 100 100
                       , ctrls = noCtrls
                       , jumpState = Stand
                       , hasBoosted = False
                       }
