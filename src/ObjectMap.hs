{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module ObjectMap
  ( theObjectMap
  ) where

import           Control.Monad.Reader (runReader)
import qualified Data.Map as M
import           GHC.TypeLits
import           Language.Haskell.Discovery
-- import           Objects.Bomb ()
import           Objects.Disco ()
-- import           Objects.Follower ()
import           Objects.Gem ()
import           Objects.WinStar ()
import           Types


allObjects :: [SomeDict1 IsObject]
allObjects = $(someDicts ''IsObject)

theObjectMap
    :: M.Map String
             ( ALens' Level (Maybe Object)
            -> V2
            -> [(String, String)]
            -> Object
             )
theObjectMap = M.fromList
             . flip fmap allObjects
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
    :: ALens' Level (Maybe Object)
    -> [(String, String)]
    -> Context a
    -> a
runInitContext lo props
  = flip runReader
  $ ObjectContext lo
                  (error "no gamestate in init")
                  props

