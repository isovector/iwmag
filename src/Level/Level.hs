module Level.Level ( Piece (Rect, Wall)
                   , defaultLevel
                   , geometry
                   , forms
                   ) where

import Math
import Utils
import FRP.Helm
import FRP.Helm.Color

data Piece = Rect Vector2 Vector2 Color
           | Wall Line Color

data Level = Level { geometry :: [Line]
                   , forms    :: [Form]
                   } deriving Show

drawLine :: Color -> Line -> Form
drawLine c (Line (pos, size@(Vector2 w _))) = move (toPair $ 0.5 |* size + pos)
                                            . filled c
                                            $ rect w 1

buildLevel :: [Piece] -> Level
buildLevel ((Wall l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geometry = l : (geometry built)
              , forms = form : (forms built)
              }
buildLevel (p:pxs)          = buildLevel pxs
buildLevel []               = Level [] []

defaultLevel = buildLevel [ Wall (lineBetween (Vector2 100 200) (Vector2 500 200)) green
                          , Wall (lineBetween (Vector2 100 500) (Vector2 500 500)) green
                          , Wall (lineBetween (Vector2 200 350) (Vector2 400 350)) red
                          ]
