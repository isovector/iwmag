module Level.Level ( Piece (Rect, Floor)
                   , defaultLevel
                   , geomWalls
                   , geomFloor
                   , forms
                   ) where

import ClassyPrelude
import Math
import Utils
import FRP.Helm
import FRP.Helm.Color

data Piece = Rect Vector2 Vector2 Color
           | Floor Line Color
           | Wall Line Color

data Level = Level { geomFloor :: [Line]
                   , geomWalls :: [Line]
                   , forms     :: [Form]
                   } deriving Show

drawLine :: Color -> Line -> Form
drawLine c (Line (pos, size)) = move (toPair pos)
                              $ traced (solid c)
                              $ segment (0,0)
                              $ toPair size

buildLevel :: [Piece] -> Level
buildLevel ((Floor l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geomFloor = l : (geomFloor built)
              , forms     = form : (forms built)
              }
buildLevel ((Wall l c):pxs) =
    let built = buildLevel pxs
        form = drawLine c l
     in built { geomWalls = l : (geomWalls built)
              , forms     = form : (forms built)
              }
buildLevel (p:pxs)          = buildLevel pxs
buildLevel []               = Level [] [] []

defaultLevel = buildLevel [ Floor (lineBetween (Vector2 100 200) (Vector2 500 200)) green
                          , Floor (lineBetween (Vector2 100 500) (Vector2 500 500)) green
                          , Floor (lineBetween (Vector2 200 350) (Vector2 400 350)) red
                          , Floor (lineBetween (Vector2 50 425)  (Vector2 250 425)) red
                          , Wall  (lineBetween (Vector2 200 350) (Vector2 200 426)) red
                          ]
