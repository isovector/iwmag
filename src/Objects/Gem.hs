{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Gem where

import Collision
import Player.Constants
import Types hiding (form)


instance IsObject "gem" where
  data InternalObj "gem" = Gem
    { gemPos :: V2
    , collected :: Bool
    , gemCol :: Color
    }
  spawn pos props = Gem pos False
                  . read
                  . ("Color " ++)
                  . fromJust
                  $ lookup "color" props
  render Gem {..} = bool form (group []) collected
    where
      form = move gemPos
           . filled gemCol
           $ polygon
           [ V2 (-4) 0
           , V2 0 (-6)
           , V2 4 0
           , V2 0 6
           ]
  update _ _ p t@Gem {..}
    | collected = t
    | otherwise =
        case withinRadius playerGeom (pPos p) 4 gemPos of
          True ->  t { collected = True }
          False -> t


