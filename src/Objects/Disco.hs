{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Disco where

import Types
import Actor.Constants


instance IsObject "disco" where
  data InternalObj "disco" = Disco
    { discoPos :: V2
    , discoDur :: Time
    , discoCol :: Color
    }
  spawn pos props = Disco (centerOnSquare 8 pos) 0
                  . read
                  . ("Color " ++)
                  . fromJust
                  $ lookup "color" props
  render Disco {..} = move (discoPos + V2 (cos discoDur) (sin discoDur) ^* 10)
                    . filled discoCol
                    $ circle 8
  update dt _ _ t@Disco {..} =
    t { discoDur = discoDur + dt * 3 }

  grasp _ _ = Nothing

