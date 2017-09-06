{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.WinStar () where

import Actor
import Actor.Constants
import Control.Monad.Writer (tell)
import Game.Sequoia.Color
import GameState (setLevel)
import Types hiding (form)


instance IsObject "winstar" where
  spawn pos props =
    defaultActor
    { aGeom = BoxGeom 7 7 7 7
    , aRender =
        \(_aPos -> p') ->
          move p' . outlined' purple
                  $ polygon
                  [ V2 0 (-7)
                  , V2 5 7
                  , V2 (-7) (-2.5)
                  , V2 (7) (-2.5)
                  , V2 (-5) 7
                  ]
    , _internal = view (from dest)
                . fromJust
                $ lookup "destination" props
    } & grabType .~ DoAction
      & aPos .~ centerOnSquare 7 pos
      & handlers . actionGrabHandler .~ moveItOrLoseIt
      & handlers . standHandler .~ pure ()


moveItOrLoseIt :: Handler ()
moveItOrLoseIt = do
  (cloneLens -> ctxSelf) <- getSelfRef
  goto <- use $ ctxSelf . internal . dest
  tell . Endo $ setLevel goto

dest :: Iso' Internal String
dest = pack

