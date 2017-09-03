{-# LANGUAGE NoImplicitPrelude #-}

module GameState
  ( GameState
  , _currentLevel
  , _player
  , _camera
  , update
  , initState
  ) where

import Actor.Constants
import Actor.Data
import Actor.Signal
import BasePrelude
import Control.Monad.State (runState)
import Game.Sequoia
import Level.Level
import Math
import Types hiding (update, to)

doorHandler :: GameState -> GameState
doorHandler s@(GameState {_player = p, _currentLevel = l}) =
    case listToMaybe $ filter (\(Door r _) -> inRect r $ _aPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Time -> Controller -> GameState -> GameState
update dt ctrl state@(GameState {_player = p, _currentLevel = l, _levelName = name}) =
    doorHandler $
      case flip runState l $ playerHandler dt state ctrl p of
        (Just p', l') ->
          let (l'', f) = updateLevel dt $ state { _player = p', _currentLevel = l' }
           in f $ state
              { _player = p'
              , _camera = _aPos p'
              , _currentLevel  = l''
              , _geometryChanged = False
              }
        (Nothing, _) -> resetState name

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { _currentLevel = l
          , _levelName = ln
          , _player = (_player s) { _aPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: String -> GameState
resetState levelname =
  let level = fromJust $ lookup levelname levels
      pos' = playerSpawn level
   in GameState
      { _currentLevel = level
      , _levelName    = levelname
      , _player       = defaultActor { _aPos = pos' }
      , _camera       = pos'
      , _geometryChanged = True
      }

initState :: GameState
initState = resetState firstLevel

