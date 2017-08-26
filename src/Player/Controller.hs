{-# LANGUAGE NoImplicitPrelude #-}

module Player.Controller ( Controller
                         , ctrlDir
                         , ctrlJump
                         , ctrlBoost
                         , ctrlSignal
                         , wantsJump
                         , wantsBoost
                         , noCtrls
                         ) where

import BasePrelude
import Game.Sequoia
import Game.Sequoia.Keyboard

data Controller =
    Controller { ctrlDir    :: !V2
               , ctrlJump   :: !Bool
               , ctrlBoost  :: !Bool
               , wantsJump  :: !Bool
               , wantsBoost :: !Bool
               } deriving (Show)

-- TODO(sandy): can we delete this?
noCtrls :: Controller
noCtrls =
    Controller { ctrlDir    = V2 0 0
               , ctrlJump   = False
               , ctrlBoost  = False
               , wantsJump  = False
               , wantsBoost = False
               }

ctrlSignal :: B [Key] -> B Controller
ctrlSignal keys = signal
  where
      makeState dir jump boost =
          Controller { ctrlDir    = dir
                     , ctrlJump   = jump
                     , ctrlBoost  = boost
                     , wantsJump  = False
                     , wantsBoost = False
                     }

      signal   = makeState <$> arrows keys <*> jumpKey keys <*> boostKey keys
      jumpKey  = flip isDown LeftShiftKey
      boostKey = flip isDown ZKey

