{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

module GameState where

import           Actor
import           Actor.Signal
import           Control.Monad.Writer (runWriterT)
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


jankyJust :: Lens' a b -> Lens' a (Maybe b)
jankyJust l = lens (pure . view l) (\a (Just b) -> a & l .~ b)


update :: Time -> Controller -> M.Map Int Controller -> GameState -> GameState
update dt ctrl ctrls gs =
  let Identity (((), gs'), appEndo -> f)
        = runWriterT
        . runHandler dt ctrl (jankyJust player) gs
        $ do
          updateLevel ctrls
          doActorHandlers
      gs'' = f gs'
   in gs'' & camera .~ gs'' ^. player . aPos


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

