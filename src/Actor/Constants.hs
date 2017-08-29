{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Constants where

import BasePrelude
import Types

targetRadius :: Double
targetRadius = 16

doubleTapTime :: Double
doubleTapTime = 0.1

jumpAttenuation :: Double
jumpAttenuation = 0.75

jumpStrength :: Double
jumpStrength = 425

jumpCount :: Int
jumpCount = 1

boostCount :: Int
boostCount = 100

boostTime :: Double
boostTime = 0.15

boostStrength :: Double
boostStrength = 112 / boostTime

boostUpPenalty :: Double
boostUpPenalty = 1.55

boostAttenuation :: Double
boostAttenuation = 0.45

recoverTime :: Double
recoverTime = 0.15

walkSpeed :: Double
walkSpeed = 120

gravity :: Double
gravity = 1000

terminalVelocity :: Double
terminalVelocity = 2000

firstLevel :: String
firstLevel = "test4"

importScale :: Double
importScale = 2.5

centerOnSquare :: Double -> V2 -> V2
centerOnSquare n pos = pos + V2 1 1 ^* (importScale * n)

