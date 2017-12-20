{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Constants where

import BasePrelude
import Types


gameWidth :: Int
gameWidth = 800

gameHeight :: Int
gameHeight = 600

targetRadius :: Double
targetRadius = 16

doubleTapTime :: Double
doubleTapTime = 0.1

jumpAttenuation :: Double
jumpAttenuation = 0.3

jumpStrength :: Double
jumpStrength = 200

jumpCount :: Int
jumpCount = 2

boostCount :: Int
boostCount = 1

boostTime :: Double
boostTime = 0.10

boostStrength :: Double
boostStrength = 30 / boostTime

boostUpPenalty :: Double
boostUpPenalty = 0.4

recoverTime :: Double
recoverTime = 0

walkSpeed :: Double
walkSpeed = 120

gravityStrength :: Double
gravityStrength = 800

terminalVelocity :: Double
terminalVelocity = 2000

firstLevel :: String
firstLevel = "test3"

importScale :: Double
importScale = 2.5

centerOnSquare :: Double -> V2 -> V2
centerOnSquare n pos = pos + V2 1 1 ^* (importScale * n)

groundFriction :: Double
groundFriction = 10

parryTime :: Double
parryTime = 0.1

