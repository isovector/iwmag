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


instance IsObject "tree" where
  data InternalObj "tree" = Tree
    { treePos :: V2
    , treeDur :: Time
    , treeCol :: Color
    }
  spawn pos props = Tree (centerOnSquare 8 pos) 0
                  . read
                  . fromJust
                  $ lookup "color" props
  render Tree {..} = move (treePos + V2 (cos treeDur) (sin treeDur) ^* 10)
                   . filled treeCol
                   $ circle 8
  update dt _ _ t@Tree {..} =
    t { treeDur = treeDur + dt * 2 }

