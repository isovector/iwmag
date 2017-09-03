{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Bomb () where

import Actor
import Actor.Constants
import Actor.Controller
import Actor.Data
import Collision (actorsIntersect)
import Actor.Signal
import Control.Lens hiding (Level)
import Control.Monad.State (evalState)
import Game.Sequoia.Color
import Object
import Types

data Bomb = Bomb
  { _held   :: Bool
  , _fActor :: Actor
  }

bombGeom :: BoxGeom
bombGeom = BoxGeom
  { topY    = 24
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

makeLenses ''Bomb

instance IsObject "bomb" where
  type InternalObj "bomb" = Bomb

  spawn pos =
    pure . Bomb False $
      defaultActor
        { _aPos = pos
        , aColor = black
        , aGeom = bombGeom
        }

  render b =
    drawWithGeom (_fActor b) $ \col size ->
      filled col . circle $ view _x size * 0.5

  update dt b = do
    gs <- asks ctxGameState
    lo <- cloneLens <$> asks ctxLens
    pure $ case view held b of
      True  -> (b, id)
      False ->
        case followerHandler dt gs makeController $ _fActor b of
          (a', Just p) ->
            ( b & fActor .~ a',
              case pieceGroup p of
                "" -> id
                g  -> (currentLevel . destructableGeometry . at g .~ Nothing)
                    . (geometryChanged .~ True)
                    . (currentLevel . lo .~ Nothing)
                    . (currentLevel .~ spawnObject "gem" (_aPos a') [("color", "0 0 1 1")] gs)
            )
          (a', _) -> (b & fActor .~ a', id)

  grasp (_fActor -> a) = do
    p  <- asks ctxPlayer
    lo <- cloneLens <$> asks ctxLens
    pure $ case actorsIntersect a p of
      True  -> Just (Bomb True a, hold lo, id)
      False -> Nothing
    where
      hold lo = Holding
       { updateHeld = \_ p' ->
           currentLevel . lo . _Just . internalObj . fActor . aPos .~ _aPos p' + V2 0 (-30)
       , onThrow = \_ dir l ->
          l & currentLevel . lo . _Just . internalObj . held   .~ False
            & currentLevel . lo . _Just . internalObj . fActor %~ setBoosting dir False throwStrength throwTime
       }


makeController :: Controller
makeController = initController
  { ctrlDir = V2 0 0
  , ctrlJump = False
  , wantsJump = False
  }


followerHandler :: Time -> GameState -> Controller -> Actor -> (Actor, Maybe Piece)
followerHandler dt gs ctrl p
   = first (fallHandler gs)
   . jumpHandler dt l ctrl
   . flip evalState gs
   $ actionHandler gs ctrl
 =<< k (walkHandler dt l ctrl)
 =<< pure p
  where
    l = _currentLevel gs
    k :: Monad m => (a -> b) -> a -> m b
    k = (pure .)

