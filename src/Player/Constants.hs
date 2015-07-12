module Player.Constants where

import ClassyPrelude
import Collision

jumpAttenuation :: Double
jumpAttenuation = 0.45

jumpStrength :: Double
jumpStrength = 300

boostTime :: Double
boostTime = 0.25

boostStrength :: Double
boostStrength = 400

prepareTime :: Double
prepareTime = 0.25

walkSpeed :: Double
walkSpeed = 200

playerGeom :: BoxGeom
playerGeom =
    BoxGeom { topY    = 20
            , bottomY = 0
            , leftX   = 10
            , rightX  = 10
            }

