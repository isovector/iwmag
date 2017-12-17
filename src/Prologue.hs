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
    :: (World EntWorld, Monad m)
    => (Entity -> Maybe a)
    -> SystemT EntWorld m [Entity]
owners f = do
  ents <- efor $ \e -> do
    with f
    pure e
  traverse getEntity ents


