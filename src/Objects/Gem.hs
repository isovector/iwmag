{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Objects.Gem () where

import Collision
import Types hiding (form)

data Gem = Gem
  { gemPos :: V2
  , gemCol :: Color
  }

instance IsObject "gem" where
  type InternalObj "gem" = Gem
  spawn pos = do
    props <- asks ctxProps
    pure . Gem pos
         . read
         . ("Color " ++)
         . fromJust
         $ lookup "color" props

  render Gem {..} = move gemPos
                  . filled gemCol
                  $ polygon
                  [ V2 (-4) 0
                  , V2 0 (-6)
                  , V2 4 0
                  , V2 0 6
                  ]
  update _ t@Gem {..} = do
    p  <- asks ctxSelf
    lo <- cloneLens <$> asks ctxLens
    pure $ case withinRadius (aGeom p) (_aPos p) 4 gemPos of
             True ->  (t, currentLevel . lo .~ Nothing)
             False -> (t, id)

  grasp _ = pure Nothing


