{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Follower () where

import Actor
import Actor.Constants
import Actor.Controller
import Actor.Data
import Actor.Signal
import Control.Lens hiding (Level)
import Control.Monad.State (evalState)
import Game.Sequoia.Color
import Linear.Metric
import Object
import Types

data Follower = Follower
  { _held       :: Bool
  , _punchTime  :: Time
  , _isPunching :: Bool
  , _fActor     :: Actor
  }

makeLenses ''Follower

instance IsObject "follower" where
  type InternalObj "follower" = Follower

  spawn pos = pure .
    Follower False 0 False $
      defaultActor
        { _aPos = pos
        , aColor = green
        }

  render Follower {..} =
    group . drawActor $ _fActor { aColor = bool (aColor _fActor) red _isPunching }

  update dt f = do
    l <- asks ctxLevel
    p <- asks ctxPlayer
    gs <- asks ctxGameState
    pure $ case view held f of
      True  -> (f, id)
      False -> punchHandler $
        f & punchTime -~ dt
          & fActor %~ \a -> followerHandler dt gs (makeController l p a) a

  grasp (_fActor -> a) = do
    p  <- asks ctxPlayer
    lo <- cloneTraversal <$> asks ctxLens
    pure $ case norm (_aPos p - _aPos a) <= 15 of
      True  -> Just (Follower True 0 False $ a { aColor = blue }, hold lo)
      False -> Nothing
    where
      hold lo = Holding
       { updateHeld = \_ p' ->
           lo . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
       , onThrow = \_ dir l ->
          l & lo . internalObj . held   .~ False
            & lo . internalObj . fActor %~ setBoosting dir False throwStrength throwTime
       }


makeController :: Level -> Actor -> Actor -> Controller
makeController _ p a = initController
  { ctrlDir =
      let dir = _aPos p - _aPos a
          dist = norm dir
       in bool (V2 0 0 ) (set _y 0 $ normalize dir) (dist >= 30)
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


punchHandler :: Follower -> (Follower, GameState -> GameState)
punchHandler f =
  case _punchTime f <= 0 of
    True  ->
      ( f & punchTime .~ punchWait
          & isPunching .~ True
      , player %~ getPunched (f ^. fActor . aPos)
      )
    False ->
      ( f & isPunching .~ False
      , id
      )


punchWait :: Double
punchWait = 2

getPunched :: V2 -> Actor -> Actor
getPunched pos a =
  let dir = _aPos a - pos
   in case norm dir < 40 of
        True -> (aHealth -~ 40) $ setBoosting (normalize dir) False throwStrength throwTime a
        False -> a

