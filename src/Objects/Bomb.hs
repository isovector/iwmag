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

  spawn pos =
    pure . Bomb False $
      defaultActor
        { _aPos = pos
        , aColor = purple
        }

  render =
    group . drawActor . _fActor

  update dt f = do
    gs <- asks ctxGameState
    pure $ case view held f of
      True  -> (f, id)
      False -> (, id) $
        f & fActor %~ \a -> followerHandler dt gs makeController a

  grasp (_fActor -> a) = do
    p  <- asks ctxPlayer
    lo <- cloneTraversal <$> asks ctxLens
    pure $ case norm (_aPos p - _aPos a) <= 15 of
      True  -> Just (Bomb True a, hold lo)
      False -> Nothing
    where
      hold lo = Holding
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


followerHandler :: Time -> GameState -> Controller -> Actor -> Actor
followerHandler dt gs ctrl p
   = fallHandler
   . (fst <$> jumpHandler dt l ctrl)
   . flip evalState l
   $ actionHandler gs ctrl
 =<< k (walkHandler dt l ctrl)
 =<< pure p
  where
    l = _currentLevel gs
    k :: Monad m => (a -> b) -> a -> m b
    k = (pure .)

