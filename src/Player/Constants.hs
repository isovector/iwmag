{-# LANGUAGE NoImplicitPrelude #-}

module Player.Constants where

import BasePrelude
import Collision
import Types

targetRadius :: Double
targetRadius = 8

doubleTapTime :: Double
doubleTapTime = 0.1

jumpAttenuation :: Double
jumpAttenuation = 0.45

jumpStrength :: Double
jumpStrength = 170

jumpCount :: Int
jumpCount = 2

boostCount :: Int
boostCount = 100

boostTime :: Double
boostTime = 0.15

boostStrength :: Double
boostStrength = 72 / boostTime

recoverTime :: Double
recoverTime = 0.45

walkSpeed :: Double
walkSpeed = 120

gravity :: Double
gravity = 700

terminalVelocity :: Double
terminalVelocity = 1400

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 24
  , bottomY = 0
  , leftX   = 8
  , rightX  = 8
  }

firstLevel :: String
firstLevel = "test3"

importScale :: Double
importScale = 2

centerOnSquare :: Double -> V2 -> V2
centerOnSquare n pos = pos + V2 1 1 ^* (importScale * n)

