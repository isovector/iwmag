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

import Player.Constants
import Player.Controller
import Player.Data
import Player.Signal
import Types hiding (update)

doorHandler :: GameState -> GameState
doorHandler s@(GameState {player = p, currentLevel = l}) =
    case listToMaybe $ filter (\(Door r _) -> inRect r $ pPos p) $ doors l of
      Just (Door _ to) -> setLevel to s
      Nothing          -> s

update :: Time -> Controller -> GameState -> GameState
update dt ctrl state@(GameState {player = p, currentLevel = l}) =
    doorHandler $ case playerHandler dt l ctrl p of
      Just p' -> state
        { player = p'
        , camera = pPos p'
        , currentLevel  = updateLevel dt p' l
        }
      Nothing -> resetState l

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { currentLevel = l
          , player = (player s) { pPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: Level -> GameState
resetState level = GameState
  { currentLevel = level
  , player       = defaultPlayer { pPos = pos' }
  , camera       = pos'
  }
  where pos' = playerSpawn level

initState :: GameState
initState = resetState . fromJust $ lookup firstLevel levels

