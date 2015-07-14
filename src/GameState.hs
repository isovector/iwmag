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
import Data.Maybe (fromJust)

import Player.Constants
import Player.Controller
import Player.Data
import Player.Signal
import Timing

data GameState =
    GameState { currentLevel :: !Level
              , player       :: !Player
              , camera       :: !Vector2
              , ctrls        :: !Controller
              }

data Update = Frame Double | Input Controller

doorHandler :: GameState -> GameState
doorHandler s@(GameState {player = p, currentLevel = l}) =
    case headMay $ filter (\(Door r _) -> inRect r $ pPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Update -> GameState -> GameState
update (Frame _) state@(GameState {player = p, ctrls = ctrl, currentLevel = l}) =
    doorHandler $ case playerHandler l ctrl p of
      Just p' -> state { player = p'
                       , camera = pPos p' * vector2X
                       , ctrls  = ctrl { wantsJump  = False
                                       , wantsBoost = False
                                       }}
      Nothing -> resetState l



update (Input ctrl') state@(GameState { ctrls = ctrl }) =
    state { ctrls = ctrl'' }
  where ctrl'' = ctrl' { wantsJump  = diff ctrlJump
                       , wantsBoost = diff ctrlBoost
                       }
        diff f = f ctrl' && not (f ctrl)

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { currentLevel = l
          , player = (player s) { pPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: Level -> GameState
resetState level =
     GameState { currentLevel = level
               , player       = defaultPlayer { pPos = playerSpawn level }
               , camera       = Vector2 0 0
               , ctrls        = noCtrls
               }

initState :: GameState
initState = resetState . fromJust $ lookup firstLevel levels

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

