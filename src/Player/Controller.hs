{-# LANGUAGE NoImplicitPrelude #-}

module Player.Controller
  ( Controller (..)
  , ctrlSignal
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

ctrlSignal :: B [Key] -> B [Key] -> B Controller
ctrlSignal keys keys' = signal
  where
      makeState dir jump' jump boost' boost =
          Controller { ctrlDir    = dir
                     , ctrlJump   = jump'
                     , ctrlBoost  = boost'
                     , wantsJump  = jump' && not jump
                     , wantsBoost = boost' && not boost
                     }

      signal   = makeState <$> arrows keys'
                           <*> jumpKey keys'
                           <*> jumpKey keys
                           <*> boostKey keys'
                           <*> boostKey keys
      jumpKey  = flip isDown LeftShiftKey
      boostKey = flip isDown ZKey

