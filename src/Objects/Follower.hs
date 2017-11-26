{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Follower () where

import Actor
import Actor.Controller
import Actor.Signal
import Control.Monad.Trans.Reader (local)
import Control.Lens hiding (Level)
import Game.Sequoia.Color
import Linear.Metric
import Types

data Follower = Follower
  { _punchTime  :: Time
  , _isPunching :: Bool
  }

makeLenses ''Follower

follower :: Iso' Internal Follower
follower = pack

instance IsObject "follower" where
  spawn pos _ =
    defaultActor
        { aColor = green
        , aRender = \a -> group
                        . drawActor
                        $ a { aColor = bool (aColor a)
                                            red
                                            (a ^. internal . follower . isPunching)
                            }
        } & handlers . updateHandler .~ update
          & handlers . walkHandler   .~ walk
          & grabType .~ Carry
          & internal . follower .~ Follower 0 False
          & aPos .~ pos


update :: Handler ()
update = do
  (cloneLens -> ctxSelf) <- getSelfRef
  dt <- view ctxTime
  me <- use ctxSelf

  when (me ^. jumpData . jumpState /= BeingHeld) $ do
    ctxSelf . internal . follower . punchTime -= dt


walk :: Handler ()
walk = do
  (cloneLens -> ctxSelf) <- getSelfRef
  me <- use ctxSelf
  p  <- use ctxPlayer
  local (ctxController .~ makeController p me) defaultWalkHandler



--   grasp (_fActor -> a) = do
--     p  <- asks ctxSelf
--     lo <- cloneLens <$> asks ctxLens
--     pure $ case actorsIntersect a p of
--       True  -> Just (Follower True 0 False $ a { aColor = blue }, hold lo, id)
--       False -> Nothing
--     where
--       hold lo = Holding
--        { updateHeld = \_ p' ->
--            currentLevel . lo . _Just . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
--        , onThrow = \_ dir gs ->
--           gs & currentLevel . lo . _Just . internalObj . held   .~ False
--              & currentLevel . lo . _Just . internalObj . fActor %~ setBoosting dir False throwStrength throwTime
--        }


makeController :: Actor -> Actor -> Controller
makeController p a = initController
  { ctrlDir =
      let dir = _aPos p - _aPos a
          dist = norm dir
       in bool (V2 0 0 ) (normalize dir & _y .~ 0) (dist >= 30)
  , ctrlJump = False
  , wantsJump = False
  }


-- followerHandler :: Time -> GameState -> Controller -> Actor -> Actor
-- followerHandler dt gs ctrl p
--    = fallHandler gs
--    . (fst <$> jumpHandler dt l ctrl)
--    . flip evalState gs
--    $ actionHandler gs ctrl
--  =<< k (walkHandler dt l ctrl)
--  =<< pure p
--   where
--     l = _currentLevel gs
--     k :: Monad m => (a -> b) -> a -> m b
--     k = (pure .)


-- punchHandler :: Follower -> (Follower, GameState -> GameState)
-- punchHandler f =
--   case _punchTime f <= 0 of
--     True  ->
--       ( f & punchTime .~ punchWait
--           & isPunching .~ True
--       , player %~ getPunched (f ^. fActor . aPos)
--       )
--     False ->
--       ( f & isPunching .~ False
--       , id
--       )


-- punchWait :: Double
-- punchWait = 2

-- getPunched :: V2 -> Actor -> Actor
-- getPunched pos a =
--   let dir = _aPos a - pos
--    in case norm dir < 40 of
--         True -> (aHealth -~ 40) $ setBoosting (normalize dir) False throwStrength throwTime a
--         False -> a

