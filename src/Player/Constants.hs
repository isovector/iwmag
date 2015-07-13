module Player.Constants where

import ClassyPrelude
import Collision

jumpAttenuation :: Double
jumpAttenuation = 0.45

jumpStrength :: Double
jumpStrength = 250

jumpCount :: Int
jumpCount = 2

boostTime :: Double
boostTime = 0.125

boostStrength :: Double
boostStrength = 800

prepareTime :: Double
prepareTime = 0.35

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

