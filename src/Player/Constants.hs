{-# LANGUAGE NoImplicitPrelude #-}

module Player.Constants where

import BasePrelude
import Collision

doubleTapTime :: Double
doubleTapTime = 0.1

jumpAttenuation :: Double
jumpAttenuation = 0.45

jumpStrength :: Double
jumpStrength = 250

jumpCount :: Int
jumpCount = 2

boostCount :: Int
boostCount = 100

boostTime :: Double
boostTime = 0.2

boostStrength :: Double
boostStrength = 80 / boostTime

recoverTime :: Double
recoverTime = 0.35

walkSpeed :: Double
walkSpeed = 200

gravity :: Double
gravity = 1300

playerGeom :: BoxGeom
playerGeom =
    BoxGeom { topY    = 16
            , bottomY = 0
            , leftX   = 4
            , rightX  = 4
            }

firstLevel :: String
firstLevel = "test1"

