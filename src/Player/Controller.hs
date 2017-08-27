{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Player.Controller
  ( Controller (..)
  , ctrlSignal
  , initController
  , foldController
  ) where

import BasePrelude
import Player.Constants
import Game.Sequoia
import Game.Sequoia.Keyboard

data RawController = RawController
  { rctrlDir        :: !V2
  , rctrlJump       :: !Bool
  , rctrlWantsJump  :: !Bool
  , rctrlWantsGrasp :: !Bool
  }

data Controller = Controller
  { ctrlDir     :: !V2
  , ctrlLastDir :: !V2
  , timeIdle    :: !Time
  , ctrlJump    :: !Bool
  , wantsJump   :: !Bool
  , wantsBoost  :: !(Maybe V2)
  , wantsGrasp  :: !Bool
  } deriving (Show)

initController :: Controller
initController = Controller
  { ctrlDir     = V2 0 0
  , ctrlLastDir = V2 0 0
  , timeIdle    = 0
  , ctrlJump    = False
  , wantsJump   = False
  , wantsBoost  = Nothing
  , wantsGrasp  = False
  }

foldController :: Time -> RawController -> Controller -> Controller
foldController dt RawController {..} Controller {..} = Controller
  { ctrlDir     = rctrlDir
  , ctrlLastDir = case (isIdle && not wasIdle, shouldBoost) of
                    (_, True)  -> V2 0 0
                    (True, _)  -> ctrlDir
                    (False, _) -> ctrlLastDir
  , timeIdle    = if isIdle
                     then timeIdle + dt
                     else 0
  , ctrlJump    = rctrlJump
  , wantsJump   = rctrlWantsJump
  , wantsBoost  = if shouldBoost
                     then Just rctrlDir
                     else Nothing
  , wantsGrasp  = rctrlWantsGrasp
  }
  where
    isIdle = rctrlDir == V2 0 0
    wasIdle = ctrlDir == V2 0 0
    shouldBoost = not isIdle && wasIdle && rctrlDir == ctrlLastDir && timeIdle <= doubleTapTime


ctrlSignal :: B [Key] -> B [Key] -> B RawController
ctrlSignal keys keys' =
  makeState <$> wasd keys'
            <*> jumpKey keys'
            <*> jumpKey keys
            <*> graspKey keys'
            <*> graspKey keys
  where
    makeState dir jump' jump grasp' grasp = RawController
      { rctrlDir        = dir
      , rctrlJump       = jump'
      , rctrlWantsJump  = jump' && not jump
      , rctrlWantsGrasp = grasp' && not grasp
      }

    jumpKey  = flip isDown LeftShiftKey
    graspKey = flip isDown EKey

