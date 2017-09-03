{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Object where

import Types
import Control.Monad.Reader (runReader)


updateObject :: Time -> GameState -> Object -> (Object, GameState -> GameState)
updateObject dt gs o@Object {..}
  = first (\obj' -> Object obj' renderObj updateObj graspObj objLens objProps)
  . runContext gs o
  $ updateObj dt obj

graspObject :: GameState -> Object -> Maybe (Object, GraspTarget)
graspObject gs o@Object {..}
  = fmap (first $ \obj' -> Object obj' renderObj updateObj graspObj objLens objProps)
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

