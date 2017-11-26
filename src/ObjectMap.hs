{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module ObjectMap
  ( theObjectMap
  ) where

import qualified Data.Map as M
import           GHC.TypeLits
import           Language.Haskell.Discovery
-- import           Objects.Bomb ()
-- import           Objects.Disco ()
-- import           Objects.Follower ()
-- -- import           Objects.Gem ()
-- import           Objects.WinStar ()
import           Types


allObjects :: [SomeDict1 IsObject]
allObjects = $(someDicts ''IsObject)

theObjectMap
    :: M.Map String
             ( V2
            -> [(String, String)]
            -> Actor
             )
theObjectMap = M.fromList
             . flip fmap allObjects
             $ withSomeDict1
             $ \(p :: Proxy name) ->
  ( symbolVal p
  , \pos props -> spawn @name pos props
  )

