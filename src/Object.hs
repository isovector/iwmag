{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Object where

import Types
import Player.Constants

updateObject :: Time -> Level -> Player -> Object -> Object
updateObject dt l p Object {..}
  = Object (updateObj dt l p obj) renderObj updateObj

renderObject :: Object -> Form
renderObject Object {..} = renderObj obj
