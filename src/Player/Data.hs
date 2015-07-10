module Player.Data ( Player
                   , pPos
                   , ctrls
                   , jumpState
                   , hasBoosted
                   , standingOn
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
           , standingOn :: Maybe Line
           } deriving (Show)

defaultPlayer :: Player
defaultPlayer = Player { pPos = Vector2 100 100
                       , ctrls = noCtrls
                       , jumpState = Stand
                       , hasBoosted = False
                       , standingOn = Nothing
                       }
