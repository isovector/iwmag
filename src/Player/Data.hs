module Player.Data ( Player
                   , pPos
                   , ctrls
                   , jumpState
                   , defaultPlayer
                   ) where

import Math
import Player.Controller
import Player.JumpState

data Player =
    Player { pPos      :: Vector2
           , ctrls     :: Controller
           , jumpState :: JumpState
           } deriving (Show)

defaultPlayer :: Player
defaultPlayer = Player { pPos = Vector2 100 100
                       , ctrls = noCtrls
                       , jumpState = Stand
                       }
