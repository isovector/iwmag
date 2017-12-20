{-# LANGUAGE NoImplicitPrelude   #-}

module Actor where

import Game.Sequoia
import Types

drawPlayer :: Color -> BoxGeom -> [Form]
drawPlayer c p = drawActor c p
            ++ [ move (V2 (-18) (-25))
               . move (V2 0 . negate $ topY p)
               . scale 0.5
               . toForm
               $ image "assets/hat.png"
               ]


drawDanger :: Time -> Form
drawDanger elapsed = rotate elapsed
                   . outlined
                       defaultLine
                         { lineColor = rgb 1 0 0
                         , lineWidth = 6
                         , lineDashing = [32, 8]
                         }
                   $ circle 32


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

playerGeom :: BoxGeom
playerGeom = BoxGeom
  { topY    = 36
  , bottomY = 0
  , leftX   = 12
  , rightX  = 12
  }

