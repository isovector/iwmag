{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Disco () where

import Actor
import Actor.Constants
import Types

instance IsObject "disco" where
  spawn pos props =
    defaultActor
    { aColor = read . ("Color " ++) . fromJust $ lookup "color" props
    , _aPos = centerOnSquare 8 pos
    , _internal = view (from disco) 0
    , aRender = renderDisco
    , aGeom = BoxGeom 0 0 0 0
    } & handlers . updateHandler .~ updateDisco

updateDisco :: Handler ()
updateDisco = do
  (cloneLens -> ctxSelf) <- getSelfRef
  dt <- asks _ctxTime
  ctxSelf . internal . pack += dt * 3

renderDisco :: Actor -> Form
renderDisco Actor {..} = move (_aPos + V2 (cos discoDur) (sin discoDur) ^* 10)
                       . filled aColor
                       $ circle 8
  where
    discoDur = view disco _internal

disco :: Iso' Internal Time
disco = pack

