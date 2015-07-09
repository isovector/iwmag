module Player.Constants ( jumpAttenuation
                        , jumpStrength
                        , boostTime
                        , boostStrength
                        , prepareTime
                        , walkSpeed
                        ) where

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
