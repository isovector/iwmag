{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}

module Prologue
  ( module Prologue
  , module Types
  , normalize
  ) where

import Types hiding (all)
import qualified Apecs.Slice as S
import Linear.Metric (normalize)


check
    :: forall c e world
     . Has world c
    => Entity e
    -> System world (Safe c)
check e = get $ cast e @c


without
    :: forall c e world
     . Has world c
    => Entity e
    -> System world Bool
without e = fmap not . exists $ cast e @c


all :: Applicative m => a -> m Bool
all = const $ pure True


mmapImpl
    :: forall world r w r' w'
     . ( Has world r'
       )
    => (Entity r' -> System world r)
    -> (Entity w' -> w -> System world ())
    -> (Entity r' -> System world Bool)
    -> (r -> System world w)
    -> System world ()
mmapImpl mget mset p f = do
  them <- owners @_ @r'
  S.forM_ them $ \who -> do
    pwho <- p who
    when pwho $ do
      dat <- mget who
      x <- f dat
      mset (cast who :: Entity w') x


mmap
    :: forall world r w
     . ( Has world r
       , Has world w
       )
    => (Entity r -> System world Bool)
    -> (r -> System world w)
    -> System world ()
mmap = mmapImpl getUnsafe set


mwmap
    :: forall world r w
     . ( Has world r
       , Has world w
       )
    => (Entity r -> System world Bool)
    -> (r -> System world (Safe w))
    -> System world ()
mwmap = mmapImpl getUnsafe set'

