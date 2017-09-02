{-# LANGUAGE NoImplicitPrelude #-}

module Actor.Constants where

import BasePrelude
import Types

targetRadius :: Double
targetRadius = 16

doubleTapTime :: Double
doubleTapTime = 0.1

jumpAttenuation :: Double
jumpAttenuation = 0.3

jumpStrength :: Double
jumpStrength = 200

jumpCount :: Int
jumpCount = 1

boostCount :: Int
boostCount = 100

boostTime :: Double
boostTime = 0.10

boostStrength :: Double
boostStrength = 40 / boostTime

throwStrength :: Double
throwStrength = 130 / throwTime

throwTime :: Double
throwTime = 0.4

boostUpPenalty :: Double
boostUpPenalty = 2.5

boostAttenuation :: Double
boostAttenuation = 0.2

recoverTime :: Double
recoverTime = 0

walkSpeed :: Double
walkSpeed = 120

gravity :: Double
gravity = 800

terminalVelocity :: Double
terminalVelocity = 2000

firstLevel :: String
firstLevel = "test2"

importScale :: Double
importScale = 2.5

centerOnSquare :: Double -> V2 -> V2
centerOnSquare n pos = pos + V2 1 1 ^* (importScale * n)

