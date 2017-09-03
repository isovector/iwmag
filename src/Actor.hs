{-# LANGUAGE NoImplicitPrelude   #-}

module Actor
  ( Actor
  , drawActor
  , drawPlayer
  , drawWithGeom
  ) where

import Actor.Data
import Actor.JumpState
import BasePrelude
import Game.Sequoia
import Linear.Vector
import Types

drawWithGeom :: Actor -> (Color -> V2 -> Form) -> Form
drawWithGeom a f = move pos
                 . move (V2 ((r - l) / 2) $ (b - t) / 2)
                 . f (aColor a)
                 $ V2 (r + l) (b + t)
  where
    pos    = _aPos a
    l      = leftX   $ aGeom a
    r      = rightX  $ aGeom a
    t      = topY    $ aGeom a
    b      = bottomY $ aGeom a

drawPlayer :: Actor -> [Form]
drawPlayer p = drawActor p
            ++ [ move (V2 (-18) (-25))
               . move (V2 0 . negate . topY $ aGeom p)
               . move (_aPos p)
               . scale 0.5
               . toForm
               $ image "assets/hat.png"
               ]

drawActor :: Actor -> [Form]
drawActor p = fmap (move pos) $
  [ move (V2 ((r - l) / 2) $ (b - t) / 2)
      . filled (aColor p)
      $ rect width height
  ]
  ++
  [ move (dir ^* (1.1 * fromIntegral (-n * (n + 3))))
      . filled (rgba 1 color 0 color)
      . circle
      $ 7 * color
  | dir <- maybeToList $ getBoostDir p
  , n   <- [0 :: Int .. 5]
  , let color = 1 - fromIntegral n / 6
  ]
  where
    pos    = _aPos p
    l      = leftX   (aGeom p)
    r      = rightX  (aGeom p)
    t      = topY    (aGeom p)
    b      = bottomY (aGeom p)
    width  = l + r
    height = t + b

getBoostDir :: Actor -> Maybe V2
getBoostDir p =
  case p ^. jumpData . jumpState of
    Boost dir _ _ _ -> Just dir
    _               -> Nothing

