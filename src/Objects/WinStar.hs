{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.WinStar () where

import Actor
import Actor.Constants
import GameState (setLevel)
import Game.Sequoia.Color
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
  goto <- gets . view $ hctxPlayer . internal . dest
  hctxState %= setLevel goto

dest :: Iso' Internal String
dest = pack

