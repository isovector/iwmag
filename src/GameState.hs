module GameState ( GameState
                 , currentLevel
                 , player
                 , camera
                 , gameSignal
                 ) where

import ClassyPrelude
import Math
import Level.Level
import FRP.Helm
import FRP.Helm.Sample hiding (update)
import FRP.Helm.Signal
import FRP.Helm.Time (inSeconds)

import Player.Controller
import Player.Data
import Player.Signal
import Timing

data GameState =
    GameState { currentLevel :: Level
              , player       :: Player
              , camera       :: Vector2
              , ctrls        :: Controller
              }

data Update = Frame Double | Input Controller

update :: Update -> GameState -> GameState
update (Frame _) state@(GameState { player = p, ctrls = ctrl }) =
    state { player = playerHandler ctrl p
          , camera = pPos p * vector2X
          , ctrls  = ctrl { wantsJump  = False
                          , wantsBoost = False
                          }
          }

update (Input ctrl') state@(GameState { ctrls = ctrl }) =
    state { ctrls = ctrl'' }
  where ctrl'' = ctrl' { wantsJump  = diff ctrlJump
                       , wantsBoost = diff ctrlBoost
                       }
        diff f = f ctrl' && not (f ctrl)

initState :: GameState
initState =
    GameState { currentLevel = defaultLevel
              , player       = defaultPlayer
              , camera       = Vector2 0 0
              , ctrls        = noCtrls
              }

gameSignal :: Signal GameState
gameSignal = foldp update initState inputSignal
  where inputSignal = merge (Input <$> ctrlSignal)
                            (Frame <$> inSeconds <$> frameRate)

-- from HEAD of Helm
merge :: Signal a -> Signal a -> Signal a
merge s1 s2 = Signal $ do
  s1' <- signalGen s1
  s2' <- signalGen s2
  return $ update' <$> s1' <*> s2'
    where update' (Changed   x) _             = Changed x
          update' (Unchanged x) (Changed y)   = Changed y
          update' (Unchanged x) (Unchanged y) = Unchanged x

