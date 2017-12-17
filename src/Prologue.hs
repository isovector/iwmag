{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Prologue
  ( module Prologue
  , module Types
  , normalize
  ) where

import Types hiding (all)
import Linear.Metric (normalize)


owners
    :: ( World EntWorld
       , Monad m
       )
    => (Entity -> Maybe a)
    -> SystemT EntWorld m [(Ent, Entity)]
owners f = do
  ents <- efor $ \e -> do
    with f
    pure e
  traverse (fmap <$> (,) <*> getEntity) ents


destroy
    :: ( World EntWorld
       , Monad m
       )
    => Ent
    -> SystemT EntWorld m ()
destroy = flip setEntity (convertSetter defEntity)

