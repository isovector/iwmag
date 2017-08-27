{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Object where

import Types


updateObject :: Time -> Level -> Actor -> Object -> Object
updateObject dt l p Object {..}
  = Object (updateObj dt l p obj) renderObj updateObj graspObj

graspObject :: Actor -> Object -> Maybe (Object, Actor -> Actor)
graspObject a Object {..} =
  fmap (first $ \obj' -> Object obj' renderObj updateObj graspObj)
    $ graspObj a obj

renderObject :: Object -> Form
renderObject Object {..} = renderObj obj

