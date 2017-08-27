{-# LANGUAGE NoImplicitPrelude #-}

module Player.JumpState
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
isBoost (Boost _ _) = True
isBoost _           = False

