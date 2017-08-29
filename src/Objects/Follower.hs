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
  { _held       :: Bool
  , _punchTime  :: Time
  , _isPunching :: Bool
  , _fActor     :: Actor
  }

makeLenses ''Follower

instance IsObject "follower" where
  type InternalObj "follower" = Follower

  spawn pos _ =
    Follower False 0 False $
      defaultActor
        { _aPos = pos
        , aColor = green
        }

  render Follower {..} =
    group . drawActor $ _fActor { aColor = bool (aColor _fActor) red _isPunching }

  update dt l p f =
    case view held f of
      True  -> (f, id)
      False -> punchHandler  $
        f & punchTime -~ dt
          & fActor %~ \a -> followerHandler dt l (makeController l p a) a

  grasp (cloneTraversal -> lo) p (_fActor -> a) =
    case norm (_aPos p - _aPos a) <= 15 of
      True  -> Just (Follower True 0 False $ a { aColor = blue }, hold)
      False -> Nothing
    where
      hold = Holding
       { updateHeld = \_ p' ->
           lo . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
       , onThrow = \_ dir l ->
          l & lo . internalObj . held   .~ False
            & lo . internalObj . fActor %~ setBoosting dir False
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


punchHandler :: Follower -> (Follower, Actor -> Actor)
punchHandler f =
  case _punchTime f <= 0 of
    True  ->
      ( f & punchTime .~ punchWait
          & isPunching .~ True
      , getPunched (f ^. fActor . aPos)
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
        True -> (aHealth -~ 40) $ setBoosting (normalize dir) False a
        False -> a

