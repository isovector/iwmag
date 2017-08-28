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
update dt ctrl state@(GameState {player = p, currentLevel = l}) =
    doorHandler $ case flip runState l $ playerHandler dt l ctrl p of
      (Just p', l') ->
        state
          { player = p'
          , camera = _aPos p'
          , currentLevel  = updateLevel dt p' l'
          }
      (Nothing, l') -> resetState l'

setLevel :: String -> GameState -> GameState
setLevel ln s
    | Just l <- lookup ln levels =
        s { currentLevel = l
          , player = (player s) { _aPos = playerSpawn l }
          }
    | otherwise = error $ "invalid level requested: " ++ ln

-- TODO: this should probably use setLevel ^^
resetState :: Level -> GameState
resetState level = GameState
  { currentLevel = level
  , player       = defaultActor { _aPos = pos' }
  , camera       = pos'
  }
  where pos' = playerSpawn level

initState :: GameState
initState = resetState . fromJust $ lookup firstLevel levels

