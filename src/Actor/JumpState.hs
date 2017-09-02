{-# LANGUAGE NoImplicitPrelude #-}

module Actor.JumpState
  ( JumpState (..)
  , isStand
  , isJump
  , isBoost
  ) where

import Types

isStand :: JumpState -> Bool
isStand Stand = True
isStand _     = False

isJump :: JumpState -> Bool
isJump (Jump _) = True
isJump _       = False

isBoost :: JumpState -> Bool
isBoost (Boost _ _ _ _ ) = True
isBoost _                = False

