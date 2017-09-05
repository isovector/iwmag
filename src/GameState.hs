{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module GameState where

import           Actor
import           Actor.Signal
import           Control.Monad.State (runState)
import qualified Data.Map as M
import           Game.Sequoia
import           Level.Level
import           Math
import           Types hiding (to)


doorHandler :: GameState -> GameState
doorHandler s@(GameState {_player = p, _currentLevel = l}) =
    case listToMaybe $ filter (\(Door r _) -> inRect r $ _aPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Time -> Controller -> M.Map Int Controller -> GameState -> GameState
update dt ctrl ctrls state@(GameState {_player = p, _levelName = name}) =
  let state' = updateLevel dt ctrls state
   in doorHandler $
        case flip runState state' . fmap pure $ runHandlers dt ctrl p of
          (Just p', gs') ->
              gs'
              { _player = p'
              , _camera = _aPos p'
              , _geometryChanged = False
              }
          (Nothing, _) -> resetState name state

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln $ levels s =
        s { _currentLevel = l
          , _levelName = ln
          , _player = (_player s) { _aPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: String -> GameState -> GameState
resetState levelname gs =
  let level = fromJust . lookup levelname $ levels gs
      pos' = playerSpawn level
   in gs
      { _currentLevel = level
      , _levelName    = levelname
      , _player       = defaultActor { _aPos = pos' }
      , _camera       = pos'
      , _geometryChanged = True
      }

