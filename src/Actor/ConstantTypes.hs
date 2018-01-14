module Actor.ConstantTypes where

data Constants = Constants
  { boostCount       :: Int
  , boostStrength    :: Double
  , boostTime        :: Double
  , boostUpPenalty   :: Double
  , doubleTapTime    :: Double
  , firstLevel       :: String
  , gameHeight       :: Int
  , gameWidth        :: Int
  , gravityStrength  :: Double
  , groundFriction   :: Double
  , importScale      :: Double
  , jumpAttenuation  :: Double
  , jumpCount        :: Int
  , jumpStrength     :: Double
  , parryTime        :: Double
  , targetRadius     :: Double
  , terminalVelocity :: Double
  , walkSpeed        :: Double
  } deriving Read

