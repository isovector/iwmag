{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.WinStar () where

import Actor.Constants
import Collision (boxesIntersect)
import Game.Sequoia.Color
import Types hiding (form)

data WinStar = WinStar
  { pos :: V2
  , destination :: String
  }

instance IsObject "winstar" where
  type InternalObj "winstar" = WinStar
  spawn pos = do
    props <- asks ctxProps
    pure . WinStar (centerOnSquare 7 pos)
         . fromJust
         $ lookup "destination" props

  render WinStar {..} = move pos
                  . outlined' purple
                  $ polygon
                  [ V2 0 (-7)
                  , V2 5 7
                  , V2 (-7) (-2.5)
                  , V2 (7) (-2.5)
                  , V2 (-5) 7
                  ]

  grasp ws@WinStar {..} = do
    p  <- asks ctxPlayer

    pure $ case boxesIntersect (aGeom p) (_aPos p) (BoxGeom 7 7 7 7) pos of
      True  -> Just (ws, Unarmed, nextLevel ?~ destination)
      False -> Nothing


