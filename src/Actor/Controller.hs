{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Actor.Controller
  ( ctrlSignal
  , initController
  , foldController
  ) where

import Actor.Constants
import Game.Sequoia.Keyboard
import Linear.Metric
import Types hiding (grasp)

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
      { rctrlDir        = normalize dir
      , rctrlJump       = jump'
      , rctrlWantsJump  = jump'  && not jump
      , rctrlWantsGrasp = grasp' && not grasp
      }

    jumpKey  = flip isDown LeftShiftKey
    graspKey = flip isDown EKey

