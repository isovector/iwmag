module Player ( Controller
              , Player
              , ctrlSignal
              , playerSignal
              , drawPlayer
              ) where

import Control.Applicative ((<$>))
import Math
import Utils
import Timing
import FRP.Helm
import FRP.Helm.Signal
import qualified FRP.Helm.Keyboard as Keyboard
import Data.Maybe (isJust, fromJust)

data Controller =
    Controller { ctrlDir   :: Vector2
               , ctrlJump  :: Bool
               , ctrlBoost :: Bool
               } deriving (Show)

data Player =
    Player { pPos      :: Vector2
           , ctrls     :: Controller
           , jumpState :: JumpState
           } deriving (Show)

data JumpState = Stand
               | Jump Double
               | Prepare Double
               | Boost Vector2 Double
               deriving (Show, Eq)

isStand :: JumpState -> Bool
isStand Stand = True
isStand _     = False

isJump :: JumpState -> Bool
isJump (Jump _) = True
isJump _       = False

isPrepare :: JumpState -> Bool
isPrepare (Prepare _) = True
isPrepare _           = False

isBoost :: JumpState -> Bool
isBoost (Boost _ _) = True
isBoost _           = False



noCtrls :: Controller
noCtrls =
    Controller { ctrlDir = Vector2 0 0
               , ctrlJump = False
               , ctrlBoost = False
               }

ctrlSignal :: Signal Controller
ctrlSignal = signal
  where
      makeState (x, y) jump boost =
          Controller { ctrlDir = Vector2 (fromIntegral x) (fromIntegral y)
                     , ctrlJump  = jump
                     , ctrlBoost = boost
                     }

      signal   = makeState <~ Keyboard.arrows ~~ jumpKey ~~ boostKey
      jumpKey  = Keyboard.isDown Keyboard.LeftShiftKey
      boostKey = Keyboard.isDown Keyboard.ZKey

canAct :: Player -> Bool
canAct p = go $ jumpState p
  where go (Boost _ _) = False
        go (Prepare _) = False
        go _           = True

drawPlayer :: Player -> Form
drawPlayer player = move (toPair . pPos $ player)
                  $ filled white
                  $ rect 10 10


jumpAttenuation :: Double
jumpAttenuation = 0.5

boostTime :: Double
boostTime = 0.25

boostStrength :: Double
boostStrength = 400

prepareTime :: Double
prepareTime = 0

jumpHandler :: Bool -> Player -> Player
jumpHandler isJumping p = go $ jumpState p
  where go (Stand)  = p

        go (Jump y)
            | isJust collided =
                p { jumpState = Stand
                  , pPos = fromJust collided
                  }
            | otherwise =
                p { jumpState = Jump $ y + (gravity') * dt
                  , pPos = (dt * y) |* vector2Y  + pPos p
                  }
          where
              gravity' = if isJumping && y < 0
                            then gravity * jumpAttenuation
                            else gravity
              collided = collision $ pPos p

        go (Prepare t)
            | t > 0     = p { jumpState = Prepare (t - dt) }
            | otherwise = p { jumpState = Boost (vnormalise . ctrlDir $ ctrls p) boostTime }

        go (Boost dir t)
            | t > 0     = p { jumpState = Boost dir (t - dt)
                            , pPos = (dt * boostStrength) |* dir + pPos p
                            }
            | otherwise = p { jumpState = Jump 0 }


collision :: Vector2 -> Maybe Vector2
collision v = if v2y v > height
                 then Just $ v { v2y = height }
                 else Nothing
  where height = 300

wasKeyJustPressed :: Bool -> Bool
wasKeyJustPressed b = b


shouldJump :: Controller -> Player -> Bool
shouldJump c p =  ( jumpState p == Stand
               || (isJump $ jumpState p))
               && wasKeyJustPressed (ctrlJump c)


playerSignal :: Signal Player
playerSignal = foldu go initialState noCtrls ctrlSignal
  where
      initialState = Player { pPos = Vector2 100 100
                            , ctrls = noCtrls
                            , jumpState = Stand
                            }

      go ctrl p =
          let speed = 200
              ctrls =
                  if canAct p
                     then ctrl
                     else noCtrls
              isJumping = shouldJump ctrls p
              jumpState' =
                  if isJumping
                     then Prepare prepareTime
                     else jumpState p

              flattened = (ctrlDir ctrls) { v2y = 0 }
           in jumpHandler (ctrlJump ctrl)
                $ p { pPos = speed * dt |* flattened + pPos p
                    , ctrls = ctrl
                    , jumpState = jumpState'
                    }

