{-# LANGUAGE NoImplicitPrelude #-}

module GameState
  ( GameState
  , currentLevel
  , player
  , camera
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
doorHandler s@(GameState {player = p, currentLevel = l}) =
    case listToMaybe $ filter (\(Door r _) -> inRect r $ _aPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Time -> Controller -> GameState -> GameState
update dt ctrl state@(GameState {player = p, currentLevel = l, levelName = name}) =
    doorHandler $
      case flip runState l $ playerHandler dt l ctrl p of
        (Just p', l') ->
          let (l'', f) = updateLevel dt p' l'
           in state
              { player = f p'
              , camera = _aPos p'
              , currentLevel  = l''
              }
        (Nothing, _) -> resetState name

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { currentLevel = l
          , levelName = ln
          , player = (player s) { _aPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: String -> GameState
resetState levelname =
  let level = fromJust $ lookup levelname levels
      pos' = playerSpawn level
   in GameState
      { currentLevel = level
      , levelName    = levelname
      , player       = defaultActor { _aPos = pos' }
      , camera       = pos'
      }

initState :: GameState
initState = resetState firstLevel

