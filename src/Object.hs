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

graspObject :: ATraversal' Level Object -> Actor -> Object -> Maybe (Object, GraspTarget)
graspObject lo a Object {..} =
  fmap (first $ \obj' -> Object obj' renderObj updateObj graspObj)
    $ graspObj lo a obj

renderObject :: Object -> Form
renderObject Object {..} = renderObj obj

internalObj :: Lens' Object (InternalObj name)
internalObj = lens (\(Object obj _ _ _) -> unsafeCoerce obj)
                   (\(Object _ a b c) obj -> Object (unsafeCoerce obj) a b c)

