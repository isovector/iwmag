{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Prologue
  ( module Prologue
  , module Types
  , normalize
  ) where

import Types hiding (all, phase)
import Linear.Metric (normalize)


owners
    :: ( Monad m
       )
    => (Entity -> Maybe a)
    -> SystemT g EntWorld m [(Ent, Entity)]
owners f = do
  ents <- efor $ \e -> do
    with f
    pure e
  traverse (fmap <$> (,) <*> getEntity) ents


destroy
    :: ( Monad m
       )
    => Ent
    -> SystemT g EntWorld m ()
destroy = flip setEntity (convertSetter defEntity)


yes :: Update ()
yes = Set ()

no :: Update a
no = Unset

