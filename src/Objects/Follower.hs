{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Follower where

import Actor
import Actor.Controller
import Actor.Data
import Actor.Signal
import Control.Lens hiding (Level)
import Game.Sequoia.Color
import Linear.Metric
import Types


instance IsObject "follower" where
  data InternalObj "follower" = Follower
    { fActor   :: Actor
    }

  spawn pos _ =
    Follower $ defaultActor
      { aPos = pos
      , aColor = green
      }

  render =
    group . drawActor . fActor

  update dt l p (fActor -> a) =
    Follower . followerHandler dt l (makeController l p a)
             $ a


makeController :: Level -> Actor -> Actor -> Controller
makeController _ p a = initController
  { ctrlDir = set _y 0 . normalize $ aPos p - aPos a
  , ctrlJump = True
  , wantsJump = True
  }


followerHandler :: Time -> Level -> Controller -> Actor -> Actor
followerHandler dt l ctrl p
  = fallHandler
  . jumpHandler dt l ctrl
  . actionHandler l ctrl
  . walkHandler dt l ctrl
  $ p

