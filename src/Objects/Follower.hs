{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Follower () where

import Control.Monad.State (evalState)
import Actor
import Actor.Controller
import Actor.Data
import Actor.Signal
import Control.Lens hiding (Level)
import Game.Sequoia.Color
import Linear.Metric
import Types
import Object

data Follower = Follower
  { _held   :: Bool
  , _fActor :: Actor
  }

makeLenses ''Follower

instance IsObject "follower" where
  type InternalObj "follower" = Follower

  spawn pos _ =
    Follower False $ defaultActor
      { _aPos = pos
      , aColor = green
      }

  render =
    group . drawActor . _fActor

  update dt l p f =
    case view held f of
      True  -> f
      False -> f & fActor %~ \a ->
        followerHandler dt l (makeController l p a) a

  grasp (cloneTraversal -> lo) p (_fActor -> a) =
    case norm (_aPos p - _aPos a) <= 15 of
      True  -> Just (Follower True $ a { aColor = blue }, hold)
      False -> Nothing
    where
      hold = Holding
       { updateHeld = \_ p' ->
           lo . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
       , onThrow = \_ dir l ->
          l & lo . internalObj . held   .~ False
            & lo . internalObj . fActor %~ setBoosting dir
       }


makeController :: Level -> Actor -> Actor -> Controller
makeController _ p a = initController
  { ctrlDir = set _y 0 . normalize $ _aPos p - _aPos a
  , ctrlJump = True
  , wantsJump = True
  }


followerHandler :: Time -> Level -> Controller -> Actor -> Actor
followerHandler dt l ctrl p
   = fallHandler
   . jumpHandler dt l ctrl
   . flip evalState l
   $ actionHandler l ctrl
 =<< k (walkHandler dt l ctrl)
 =<< pure p
  where
    k :: Monad m => (a -> b) -> a -> m b
    k = (pure .)

