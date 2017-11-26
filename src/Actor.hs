{-# LANGUAGE NoImplicitPrelude   #-}

module Actor where

import Actor.Constants
import Actor.Controller
import Actor.JumpState
import Game.Sequoia
import Game.Sequoia.Color
import Actor.Signal
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

drawPlayer :: Color -> BoxGeom -> [Form]
drawPlayer c p = drawActor c p
            ++ [ move (V2 (-18) (-25))
               . move (V2 0 . negate $ topY p)
               . scale 0.5
               . toForm
               $ image "assets/hat.png"
               ]

drawActor :: Color -> BoxGeom -> [Form]
drawActor c p =
  [ move (V2 ((r - l) / 2) $ (b - t) / 2)
      . filled c
      $ rect width height
  ]
  -- ++
  -- [ move (dir ^* (1.1 * fromIntegral (-n * (n + 3))))
  --     . filled (rgba 1 color 0 color)
  --     . circle
  --     $ 7 * color
  -- | dir <- maybeToList $ getBoostDir p
  -- , n   <- [0 :: Int .. 5]
  -- , let color = 1 - fromIntegral n / 6
  -- ]
  where
    l      = leftX   p
    r      = rightX  p
    t      = topY    p
    b      = bottomY p
    width  = l + r
    height = t + b

getBoostDir :: Actor -> Maybe V2
getBoostDir p =
  case p ^. jumpData . jumpState of
    Boost dir _ _ _ -> Just dir
    _               -> Nothing

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 36
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

