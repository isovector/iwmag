{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Gem () where

import Collision
import Types hiding (form)

data Gem = Gem
  { gemPos :: V2
  , collected :: Bool
  , gemCol :: Color
  }

instance IsObject "gem" where
  type InternalObj "gem" = Gem
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
    | collected = (t, id)
    | otherwise =
        (, id) $ case withinRadius (aGeom p) (_aPos p) 4 gemPos of
          True ->  t { collected = True }
          False -> t

  grasp _ _ _ = Nothing


