{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Prologue
  ( module Prologue
  , module Types
  , normalize
  ) where

import           Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State as S
import           Linear.Metric (normalize)
import           Types hiding (all, phase)


owners
    :: ( Monad m
       )
    => (Entity -> Maybe a)
    -> SystemT EntWorld m [(Ent, Entity)]
owners f = do
  ents <- efor $ \e -> do
    with f
    pure e
  traverse (fmap <$> (,) <*> getEntity) ents


destroy
    :: ( Monad m
       )
    => Ent
    -> SystemT EntWorld m ()
destroy = flip setEntity (convertSetter defEntity)


yes :: Update ()
yes = Set ()


no :: Update a
no = Unset


------------------------------------------------------------------------------
-- | Effective velocity
evel :: Entity -> Maybe V2
evel Entity {..} = asum
  [ (+) <$> vel
        <*> xvel
  , vel
  , xvel
  ]

modifyGlobals :: (Globals -> Globals) -> Sys ()
modifyGlobals = lift . S.modify

getGlobal :: (Globals -> a) -> Sys a
getGlobal = lift . S.gets

zipAssocWith
    :: Ord k
    => (a -> b -> c)
    -> [(k, a)]
    -> [(k, b)]
    -> [(k, c)]
zipAssocWith _ [] _ = []
zipAssocWith _ _ [] = []
zipAssocWith f l@((i,a):l') m@((j,b):m')
    | i < j = zipAssocWith f l' m
    | i > j = zipAssocWith f l m'
    | otherwise = (i, f a b) : zipAssocWith f l' m'

