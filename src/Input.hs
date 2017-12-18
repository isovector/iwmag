{-# LANGUAGE NoImplicitPrelude #-}

module Input where

import Game.Sequoia.Keyboard hiding (isDown, arrows)
import Prologue


getKeys :: Sys [Key]
getKeys = liftIO $ fmap toEnum <$> getKeyState


input :: [Key] -> (V2, Bool)
input keys =
  let isDown = flip elem keys
      l = isDown LeftKey
      r = isDown RightKey
      u = isDown UpKey
      d = isDown DownKey
      j = isDown LeftShiftKey
   in
    ( uncurry V2
      ( fromIntegral $ -1 * fromEnum l + 1 * fromEnum r
      , fromIntegral $ -1 * fromEnum u + 1 * fromEnum d
      )
    , j
    )

