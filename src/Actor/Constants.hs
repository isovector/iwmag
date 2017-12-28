{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Actor.Constants where

import qualified Actor.ConstantTypes as CT
import BasePrelude
import Types

constants :: CT.Constants
constants = read . unsafePerformIO $ readFile "assets/constants"
{-# NOINLINE constants #-}

CT.Constants {..} = constants

centerOnSquare :: Double -> V2 -> V2
centerOnSquare n pos = pos + V2 1 1 ^* (importScale * n)

