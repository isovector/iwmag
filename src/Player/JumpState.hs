module Player.JumpState ( JumpState (Stand, Jump, Prepare, Boost)
                        , isStand
                        , isJump
                        , isPrepare
                        , isBoost
                        ) where

import ClassyPrelude
import Math

data JumpState = Stand
               | Jump Double
               | Prepare Double
               | Boost Vector2 Double
               deriving (Show, Eq)

isStand :: JumpState -> Bool
isStand Stand = True
isStand _     = False

isJump :: JumpState -> Bool
isJump (Jump _) = True
isJump _       = False

isPrepare :: JumpState -> Bool
isPrepare (Prepare _) = True
isPrepare _           = False

isBoost :: JumpState -> Bool
isBoost (Boost _ _) = True
isBoost _           = False

