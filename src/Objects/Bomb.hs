{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Bomb () where

import Control.Monad.State (evalState)
import Actor
import Actor.Controller
import Actor.Constants
import Actor.Data
import Actor.Signal
import Control.Lens hiding (Level)
import Game.Sequoia.Color
import Linear.Metric
import Types
import Object

data Bomb = Bomb
  { _held   :: Bool
  , _fActor :: Actor
  }

makeLenses ''Bomb

instance IsObject "bomb" where
  type InternalObj "bomb" = Bomb

  spawn pos _ =
    Bomb False $
      defaultActor
        { _aPos = pos
        , aColor = purple
        }

  render =
    group . drawActor . _fActor

  update dt l _ f =
    case view held f of
      True  -> (f, id)
      False -> (, id) $
        f & fActor %~ \a -> followerHandler dt l makeController a

  grasp (cloneTraversal -> lo) p (_fActor -> a) =
    case norm (_aPos p - _aPos a) <= 15 of
      True  -> Just (Bomb True a, hold)
      False -> Nothing
    where
      hold = Holding
       { updateHeld = \_ p' ->
           lo . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
       , onThrow = \_ dir l ->
          l & lo . internalObj . held   .~ False
            & lo . internalObj . fActor %~ setBoosting dir False throwStrength throwTime
       }


makeController :: Controller
makeController = initController
  { ctrlDir = V2 0 0
  , ctrlJump = False
  , wantsJump = False
  }


followerHandler :: Time -> Level -> Controller -> Actor -> Actor
followerHandler dt l ctrl p
   = fallHandler
   . (fst <$> jumpHandler dt l ctrl)
   . flip evalState l
   $ actionHandler l ctrl
 =<< k (walkHandler dt l ctrl)
 =<< pure p
  where
    k :: Monad m => (a -> b) -> a -> m b
    k = (pure .)

