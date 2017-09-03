{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module ObjectMap
  ( objectMap
  ) where

import           Control.Monad.Reader (runReader)
import qualified Data.Map as M
import           GHC.TypeLits
import           Language.Haskell.Discovery
import           Objects.Bomb ()
import           Objects.Disco ()
import           Objects.Follower ()
import           Objects.Gem ()
import           Types


allObjects :: [SomeDict1 IsObject]
allObjects = $(someDicts ''IsObject)

ffmap :: Functor f => f a -> (a -> b) -> f b
ffmap = flip fmap

objectMap
    :: M.Map String
             ( ATraversal' Level Object
            -> V2
            -> [(String, String)]
            -> Object
             )
objectMap = M.fromList . ffmap allObjects
                       $ withSomeDict1
                       $ \(p :: Proxy name) ->
  ( symbolVal p
  , \lo pos props ->
      Object (runInitContext lo props $ spawn @name pos)
             render
             update
             grasp
             lo
             props
  )


runInitContext
    :: ATraversal' Level Object
    -> [(String, String)]
    -> Context a
    -> a
runInitContext lo props
  = flip runReader
  $ ObjectContext lo
                  (error "no gamestate in init")
                  props

