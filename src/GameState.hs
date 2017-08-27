{-# LANGUAGE NoImplicitPrelude #-}

module GameState
  ( GameState
  , currentLevel
  , player
  , camera
  , update
  , initState
  ) where

import BasePrelude
import Math
import Level.Level
import Game.Sequoia

import Actor.Constants
import Actor.Controller
import Actor.Data
import Actor.Signal
import Types hiding (update)

doorHandler :: GameState -> GameState
doorHandler s@(GameState {player = p, currentLevel = l}) =
    case listToMaybe $ filter (\(Door r _) -> inRect r $ aPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Time -> Controller -> GameState -> GameState
update dt ctrl state@(GameState {player = p, currentLevel = l}) =
    doorHandler $ case playerHandler dt l ctrl p of
      Just p' -> state
        { player = p'
        , camera = aPos p'
        , currentLevel  = updateLevel dt p' l
        }
      Nothing -> resetState l

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { currentLevel = l
          , player = (player s) { aPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: Level -> GameState
resetState level = GameState
  { currentLevel = level
  , player       = defaultActor { aPos = pos' }
  , camera       = pos'
  }
  where pos' = playerSpawn level

initState :: GameState
initState = resetState . fromJust $ lookup firstLevel levels

