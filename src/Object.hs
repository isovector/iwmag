{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Object where

import           Control.Monad.Reader (runReader)
import qualified Data.Map as M
import           GHC.TypeLits
import           Types


updateObject :: Time -> GameState -> Object -> (Object, GameState -> GameState)
updateObject dt gs o@Object {..}
  = first (\obj' -> Object obj' renderObj updateObj graspObj objLens objProps)
  . runContext gs o
  $ updateObj dt obj

graspObject :: GameState -> Object -> Maybe (Object, GraspTarget, GameState -> GameState)
graspObject gs o@Object {..}
  = fmap (_1 %~ \obj' -> Object obj' renderObj updateObj graspObj objLens objProps)
  . runContext gs o
  $ graspObj obj

renderObject :: Object -> Form
renderObject Object {..}
  = renderObj obj

internalObj :: Lens' Object (InternalObj name)
internalObj = lens (\(Object obj _ _ _ _ _) -> unsafeCoerce obj)
                   (\(Object _ a b c d e) obj -> Object (unsafeCoerce obj) a b c d e)

runContext :: GameState -> Object -> Context a -> a
runContext gs obj =
  flip runReader $ ObjectContext (objLens obj)
                                 gs
                                 (objProps obj)

spawnObject
    :: String
    -> V2
    -> [(String, String)]
    -> GameState
    -> Level
spawnObject objtype pos props gs =
  let l   = _currentLevel gs
      idx = (+ 1) . maximum $ 0 : M.keys (_objects l)
      lo  = objects . at idx
   in l & cloneLens lo ?~ (objectMap gs M.! objtype) lo pos props


spawnObject'
    :: forall name
     . IsObject name
    => V2
    -> [(String, String)]
    -> GameState
    -> Level
spawnObject' = spawnObject (symbolVal $ Proxy @name)

