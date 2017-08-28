{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module ObjectMap
  ( objectMap
  ) where

import qualified Data.Map as M
import           GHC.TypeLits
import           Language.Haskell.Discovery
import           Objects.Disco ()
import           Objects.Follower ()
import           Objects.Gem ()
import           Types


allObjects :: [SomeDict1 IsObject]
allObjects = $(someDicts ''IsObject)

ffmap :: Functor f => f a -> (a -> b) -> f b
ffmap = flip fmap

objectMap :: M.Map String (V2 -> [(String, String)] -> Object)
objectMap = M.fromList . ffmap allObjects
                       $ withSomeDict1
                       $ \(p :: Proxy name) ->
  ( symbolVal p
  , \pos props ->
      Object (spawn @name pos props)
             render
             update
             grasp
  )

