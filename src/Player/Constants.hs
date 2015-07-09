module Player.Constants ( jumpAttenuation
                        , jumpStrength
                        , boostTime
                        , boostStrength
                        , prepareTime
                        ) where

jumpAttenuation :: Double
jumpAttenuation = 0.5

jumpStrength :: Double
jumpStrength = -200

boostTime :: Double
boostTime = 0.25

boostStrength :: Double
boostStrength = 400

prepareTime :: Double
prepareTime = 0
